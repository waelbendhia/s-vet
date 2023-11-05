{-# LANGUAGE AllowAmbiguousTypes #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module SVet.Tracing (
  runSeldaTracing,
  runTracing,
  Traced,
  Tracing (..),
  TracedAPI,
  withTracer,
  withSpan,
) where

import Data.Vault.Lazy
import qualified Database.Selda.Backend.Internal as Selda
import GHC.Exception
import GHC.TypeLits
import Network.Wai
import qualified OpenTelemetry.Context as Otel
import qualified OpenTelemetry.Context.ThreadLocal as Otel
import qualified OpenTelemetry.Trace as Otel
import qualified OpenTelemetry.Trace.Core as Otel
import qualified OpenTelemetry.Trace.Monad as Otel
import Polysemy
import Polysemy.Input
import Polysemy.Resource
import Relude
import SVet.Selda
import Servant
import Servant.Auth
import Servant.Server.Internal.Delayed

data Tracing m a where
  InSpan :: CallStack -> Text -> Otel.SpanArguments -> m a -> Tracing m a
  WithSpan :: Otel.Span -> m a -> Tracing m a
  CurrentSpan :: Tracing m (Maybe Otel.Span)
  WithTracer :: Otel.Tracer -> m a -> Tracing m a
  GetTracer :: Tracing m Otel.Tracer
  GetTracerProvider :: Tracing m Otel.TracerProvider

makeSem ''Tracing

instance (Member (Input Otel.Tracer) r) => Otel.MonadTracer (Sem r) where getTracer = input

data Traced

type family TracedAPI api where
  TracedAPI (a :<|> b) = TracedAPI a :<|> TracedAPI b
  TracedAPI a = Traced :> a

instance
  (HasServer api ctx, HasContextEntry ctx (Key Otel.Span), Nameable api) =>
  HasServer (Traced :> api) ctx
  where
  type ServerT (Traced :> api) m = ServerT api m
  route _ ctx del =
    route (Proxy @api) ctx $
      del `addMethodCheck` do
        mtp <- asks (lookup (getContextEntry ctx) . vault)
        msp <-
          maybe
            do
              mctx <- Otel.lookupContext
              pure $ Otel.lookupSpan =<< mctx
            (pure . Just)
            mtp
        msp `forM_` (`Otel.updateName` name @api)

  hoistServerWithContext _ = hoistServerWithContext (Proxy @api)

class Nameable api where
  name :: Text

instance (KnownSymbol sym, Nameable api) => Nameable (sym :> api) where
  name = toText (symbolVal @sym Proxy) <> "/" <> name @api

instance (Nameable api) => Nameable (Auth auths authResult :> api) where name = name @api

instance (Nameable api) => Nameable (ReqBody types body :> api) where name = name @api

instance Nameable (Post types resp) where name = " POST"

instance Nameable (Get types resp) where name = " GET"

instance Nameable (Put types resp) where name = " PUT"

instance Nameable (Delete types resp) where name = " DELETE"

instance (Nameable api) => Nameable (QueryParam' mods sym t :> api) where
  name = name @api

instance (KnownSymbol sym, Nameable api) => Nameable (Capture' mods sym t :> api) where
  name = "{" <> toText (symbolVal @sym Proxy) <> "}/" <> name @api

inSpan' :: (Members '[Tracing] r) => Text -> Otel.SpanArguments -> Sem r a -> Sem r a
inSpan' = inSpan callStack

runTracing ::
  forall r a.
  (Members [Resource, Embed IO] r) =>
  Otel.TracerProvider ->
  Sem (Tracing : r) a ->
  Sem r a
runTracing tp = runTracing' Nothing Nothing
 where
  runTracing' :: forall b. Maybe Otel.Tracer -> Maybe Otel.Span -> Sem (Tracing : r) b -> Sem r b
  runTracing' mt ms =
    interpretH \case
      GetTracerProvider -> pureT tp
      WithTracer t m -> do
        mm <- runT m
        raise $ runTracing' (Just t) ms mm
      WithSpan sp m -> do
        mm <- runT m
        raise $ runTracing' mt (Just sp) mm
      GetTracer ->
        pureT $
          fromMaybe
            (Otel.makeTracer tp "opentelemetry-s-vet" (Otel.TracerOptions Nothing))
            mt
      CurrentSpan -> case ms of
        Just sp -> pureT $ Just sp
        Nothing -> do
          mctx <- Otel.lookupContext
          pureT $ Otel.lookupSpan =<< mctx
      InSpan cs n args m -> do
        mm <- runT m
        t <- raise $ runTracing' mt ms getTracer
        bracket
          startSpan
          (uncurry endSpan)
          \(_, s) -> raise (runTracing' (Just t) (Just s) mm) `onException` onError s
       where
        startSpan = do
          ctx <- Otel.getContext
          t <- raise $ runTracing' mt ms getTracer
          s <- Otel.createSpanWithoutCallStack t ctx n args
          Otel.adjustContext (Otel.insertSpan s)
          Otel.whenSpanIsRecording s $ case getCallStack cs of
            [] -> pass
            (fn, loc) : _ -> do
              Otel.addAttributes
                s
                ( fromList
                    [ ("code.function", Otel.toAttribute $ toText fn)
                    , ("code.namespace", Otel.toAttribute $ toText $ srcLocModule loc)
                    , ("code.filepath", Otel.toAttribute $ toText $ srcLocFile loc)
                    , ("code.lineno", Otel.toAttribute $ srcLocStartLine loc)
                    , ("code.package", Otel.toAttribute $ toText $ srcLocPackage loc)
                    ]
                )
          mParent <- raise $ runTracing' mt ms currentSpan
          pure (mParent <|> Otel.lookupSpan ctx, s)
        onError s = Otel.setStatus s $ Otel.Error ""
        endSpan parent s = do
          Otel.endSpan s Nothing
          Otel.adjustContext \ctx -> maybe (Otel.removeSpan ctx) (`Otel.insertSpan` ctx) parent

runSeldaTracing ::
  (Members '[Tracing, Selda, Final IO, Resource] r) =>
  Sem r a ->
  Sem r a
runSeldaTracing = interceptH @Selda \case
  WithConnection takesConnection -> do
    takesConnectionT <- bindT takesConnection
    send $ WithConnection \c -> do
      c' <- instrumentConnection c
      mc' <- pureT c'
      raise . subsume $ takesConnectionT mc'
  UseConnection c m -> do
    mm <- runT m
    c' <- instrumentConnection c
    useConnection c' (raise $ subsume mm)
  Transact m -> do
    mm <- runT m
    send $ WithConnection \c -> do
      c' <- instrumentConnection c
      useConnection c' $ send $ Transact $ raise $ subsume mm
 where
  instrumentConnection c = do
    tp <- getTracerProvider
    let withSQLSpan :: forall a. [(Text, Otel.Attribute)] -> IO a -> IO a
        withSQLSpan attrs =
          runM @IO
            . runResource
            . runTracing tp
            . inSpan'
              "sql.statement.query"
              Otel.defaultSpanArguments
                { Otel.kind = Otel.Client
                , Otel.attributes = fromList attrs
                }
            . embed
        prevBackend = Selda.connBackend c
    pure
      c
        { Selda.connBackend =
            prevBackend
              { Selda.runStmt = \q params -> do
                  withSQLSpan
                    [("db.statement", fromString $ toString q)]
                    $ Selda.runStmt prevBackend q params
              , Selda.runStmtWithPK = \q params -> do
                  withSQLSpan
                    [("db.statement", fromString $ toString q)]
                    $ Selda.runStmtWithPK prevBackend q params
              , Selda.prepareStmt = \stId params q ->
                  withSQLSpan
                    [("db.statement", fromString $ toString q)]
                    $ Selda.prepareStmt prevBackend stId params q
              }
        }
