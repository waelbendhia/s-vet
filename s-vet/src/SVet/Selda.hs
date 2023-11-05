{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module SVet.Selda (
  ConnectionPool,
  Selda (..),
  runSelda,
  transaction,
  useConnection,
) where

import Data.Pool
import Database.Selda hiding (transaction)
import qualified Database.Selda as Selda
import qualified Database.Selda.Backend as Selda
import qualified Database.Selda.PostgreSQL as Selda
import Polysemy
import Polysemy.Error
import Polysemy.Resource
import Relude hiding (Reader, ask, fromException, local)

type ConnectionPool = Pool (Selda.SeldaConnection Selda.PG)

withConnectionFromPool ::
  (Members '[Resource, Final IO] r) =>
  ConnectionPool ->
  (Selda.SeldaConnection Selda.PG -> Sem r b) ->
  Sem r b
withConnectionFromPool p f =
  bracket
    (embedFinal $ takeResource p)
    (embedFinal . uncurry (flip putResource))
    (f . fst)

data Selda m a where
  WithConnection :: (Selda.SeldaConnection Selda.PG -> m a) -> Selda m a
  UseConnection :: Selda.SeldaConnection Selda.PG -> m a -> Selda m a
  Transact :: m a -> Selda m a

makeSem ''Selda

runSelda' ::
  Members '[Final IO, Resource, Error SeldaError] r =>
  ConnectionPool ->
  Maybe (Selda.SeldaConnection Selda.PG) ->
  Sem (Selda : r) a ->
  Sem r a
runSelda' p mc = interpretH \case
  WithConnection takesConnection -> do
    tc <- bindT takesConnection
    case mc of
      Just c -> do
        connWithContext <- pureT c
        raise $ fromExceptionSem @SeldaError $ runSelda' p (Just c) $ tc connWithContext
      Nothing -> withConnectionFromPool p \c -> do
        connWithContext <- pureT c
        raise $ fromExceptionSem @SeldaError $ runSelda' p (Just c) $ tc connWithContext
  UseConnection c m -> do
    mm <- runT m
    raise $ fromExceptionSem @SeldaError $ runSelda' p (Just c) mm
  Transact m -> do
    mm <- runT m
    case mc of
      Nothing -> withConnectionFromPool p \c ->
        raise $ fromExceptionSem @SeldaError $ runSelda' p (Just c) mm
      Just c -> raise $ fromExceptionSem @SeldaError $ runSelda' p (Just c) mm

runSelda ::
  Members '[Final IO, Resource, Error SeldaError] r =>
  ConnectionPool ->
  Sem (Selda : r) a ->
  Sem r a
runSelda p = runSelda' p Nothing

instance Members '[Selda, Embed IO] r => Selda.MonadSelda (Sem r) where
  type Backend (Sem r) = Selda.PG
  withConnection = withConnection
  transact = transact

transaction :: (Selda.MonadSelda (Sem r), Member Resource r) => Sem r a -> Sem r a
transaction m = Selda.transact do
  void $ exec "BEGIN TRANSACTION"
  onException
    do
      r <- m
      void $ exec "COMMIT"
      pure r
    (exec "ROLLBACK")
 where
  exec q = Selda.withBackend $ \b -> liftIO $ fst <$> Selda.runStmt b q []
