module SVet (startApp, app) where

import qualified Control.Exception as E
import Control.Monad.Catch hiding (Handler, onException)
import Crypto.BCrypt
import Crypto.Number.Generate
import Data.Pool
import Data.Time.Clock
import qualified Data.Vault.Lazy as Vault
import Database.Selda hiding (first, toString, toText, transaction)
import Database.Selda.Backend hiding (toText)
import Database.Selda.PostgreSQL
import Katip hiding (KatipContext)
import Network.URI
import Network.Wai
import qualified Network.Wai.Handler.Warp as Warp
import OpenTelemetry.Instrumentation.Wai
import qualified OpenTelemetry.Trace as T
import Options.Generic
import Polysemy
import Polysemy.Error
import Polysemy.Input
import Polysemy.Resource
import Relude hiding (MonadReader, Reader, ask, asks, local, runReader)
import SVet.API
import SVet.API.Acts
import SVet.Authentication
import SVet.Domain.Authentication (defaultEmail)
import SVet.Logging
import SVet.Repository
import SVet.Selda
import SVet.ServerError
import SVet.Timing
import SVet.Tracing
import Servant
import Servant.Auth.Server

api :: Proxy API
api = Proxy

data SVetConfig = SVetConfig {port :: !Int, dbUrl :: !Text, dbPool :: !Int}
  deriving (Read, Show, Eq, Generic)

instance ParseRecord SVetConfig

loggingMiddleware :: LogEnv -> Namespace -> Application -> Application
loggingMiddleware le ns app' req =
  runKatipContextT @SimpleLogPayload le mempty ns . logSemy message . liftIO . app' req
 where
  message = unwords ["new request", show (requestMethod req) <> ":", show (rawPathInfo req)]

uriToConnectInfo :: Text -> Maybe PGConnectInfo
uriToConnectInfo u = do
  (URI _ auth' path' _ _) <- parseURI $ toString u
  (URIAuth user host sPort) <- auth'
  let username = toText $ takeWhile (/= ':') user
      password' =
        viaNonEmpty init <=< viaNonEmpty tail $
          dropWhile (/= ':') user
  iPort <- readMaybe @Int =<< viaNonEmpty tail sPort
  iPath <- viaNonEmpty tail path'
  pure $
    PGConnectInfo
      (toText host)
      iPort
      (toText iPath)
      Nothing
      (Just username)
      (toText <$> password')

createConnectionPool :: PGConnectInfo -> Int -> IO (Pool (SeldaConnection PG))
createConnectionPool connectInfo poolSize =
  newPool $
    defaultPoolConfig
      (pgOpen connectInfo)
      seldaClose
      600
      poolSize

data AppError
  = HashError !HashingError
  | Pet !PetError
  | Treatment !TreatmentError
  | Consultation !ConsultationError
  | Act !ActError
  | Owner !OwnerError
  | Statistics !StatisticsError
  | Authentication !AuthenticationError
  | ActValidation !ActValidationError
  | Selda !SeldaError
  deriving (Show, Generic)

instance ToServerError AppError where
  toServerError (HashError _) = err500
  toServerError (Pet (PEPetNotFound _)) = err404{errReasonPhrase = "pet not found"}
  toServerError (Pet (PEOwnerNotFound _ _)) = err404{errReasonPhrase = "owner not found"}
  toServerError (Treatment (TEConsultationNotFound _ _)) =
    err404{errReasonPhrase = "consultation not found"}
  toServerError (Consultation (CEPetNotFound _ _)) = err404{errReasonPhrase = "pet not found"}
  toServerError (Consultation (CEConsultationNotFound _)) =
    err404{errReasonPhrase = "consultation not found"}
  toServerError (Act (AEActNotFound _ _)) = err404{errReasonPhrase = "act not found"}
  toServerError (ActValidation InvalidName) = err404{errReasonPhrase = "invalid name"}
  toServerError (ActValidation InvalidPrice) = err404{errReasonPhrase = "invalid price"}
  toServerError (Owner (OEOwnerNotFound _ _)) = err404{errReasonPhrase = "owner not found"}
  toServerError (Statistics (SEStatisticsNoRows{})) = err500
  toServerError (Authentication _) = err401
  toServerError (Selda (DbError msg)) = err500{errReasonPhrase = msg}
  toServerError (Selda (SqlError msg)) = err400{errReasonPhrase = msg}
  toServerError (Selda (UnsafeError msg)) = err500{errReasonPhrase = msg}

runRandomIntInput :: (Members '[Embed IO] r) => Sem (Input Int : r) a -> Sem r a
runRandomIntInput = runInputSem $ embed @IO $ fromIntegral <$> generateMax 1000000

app :: Int -> LogEnv -> Pool (SeldaConnection PG) -> T.TracerProvider -> IO Application
app lp le connPool tp = do
  jwtSettings' <- defaultJWTSettings <$> generateKey
  let runLogSimple :: (Katip.LogItem i) => i -> Sem '[KatipContext, Embed IO] () -> IO ()
      runLogSimple li = runM . runKatipContext le (liftPayload li) "s-vet"
  withResource connPool $ \p -> do
    runLogSimple () $ $logTM DebugS "running migration"
    runSeldaT migrateUp p
      `catchAll` \e -> do
        liftIO
          ( runLogSimple (sl "error" $ show @Text e) $
              $logTM DebugS "migration failed running anyway"
          )
  spanKey <- Vault.newKey @T.Span
  let cookieSettings' =
        defaultCookieSettings
          { cookieIsSecure = NotSecure
          , cookieSameSite = SameSiteStrict
          , cookieXsrfSetting = Nothing
          , sessionCookieName = "access_token"
          }
      context = cookieSettings' :. jwtSettings' :. spanKey :. EmptyContext
  _ <- runLogSimple (sl "port" lp) $ $logTM InfoS "starting server"
  otelMiddleware <- newOpenTelemetryWaiMiddleware
  initResult <-
    runFinal
      . runResource
      . embedToFinal @IO
      . runError @AppError
      . mapError Selda
      . mapError HashError
      . runCrypt fastBcryptHashingPolicy
      . runInputSem (embed getCurrentTime)
      . runSelda connPool
      . runKatipContext le mempty "s-vet"
      . runSettingsRepository
      $ initSettings defaultEmail
  either (error . show) pure initResult
  pure $
    otelMiddleware $
      loggingMiddleware le "s-vet" $
        serveWithContext api context $
          hoistServerWithContext
            api
            (Proxy @'[CookieSettings, JWTSettings, Vault.Key T.Span])
            ( Handler
                . ExceptT
                . runFinal
                . embedToFinal @IO
                . runResource
                . runKatipContext le mempty "s-vet"
                . runTracing tp
                . runError @ServerError
                . mapError @AppError toServerError
                . mapError ActValidation
                . mapError Authentication
                . mapError HashError
                . mapError Consultation
                . mapError Owner
                . mapError Pet
                . mapError Treatment
                . mapError Statistics
                . mapError Act
                . mapError Selda
                . runRandomIntInput
                . runInputSem (embed getCurrentTime)
                . runSession cookieSettings' jwtSettings'
                . runCrypt fastBcryptHashingPolicy
                . runSelda connPool
                . runSeldaTracing
                . runConsultationRepository
                . runOwnerRepository
                . runPetRepository
                . runSettingsRepository
                . runTreatmentRepository
                . runStatistics
                . runActRepository
                . transaction
            )
            server

startApp :: SVetConfig -> IO ()
startApp (SVetConfig p db poolSize) = do
  handleScribe <-
    mkHandleScribeWithFormatter
      jsonFormat
      ColorIfTerminal
      stdout
      (const $ pure True)
      V3
  let mkLogEnv =
        registerScribe "stdout" handleScribe defaultScribeSettings
          =<< initLogEnv "s-vet" "production"
      withLogEnv = E.bracket mkLogEnv closeScribes
  withLogEnv \le -> withConnPool \pool -> withTracerProvider (Warp.run p <=< app p le pool)
 where
  dbConfig = uriToConnectInfo db ?: error "could not parse db uri"
  withConnPool =
    E.bracket
      (createConnectionPool dbConfig poolSize)
      destroyAllResources
  withTracerProvider =
    E.bracket
      T.initializeGlobalTracerProvider
      T.shutdownTracerProvider
