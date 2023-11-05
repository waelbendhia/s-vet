{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Main where

import qualified Control.Exception as CE
import Control.Monad.Catch
import Crypto.BCrypt
import Data.Aeson
import Data.Aeson.Optics as AO
import Database.Selda hiding (first, toString, toText)
import Database.Selda.PostgreSQL
import Network.URI hiding (query)
import Optics
import Options.Generic
import Relude
import SVet.Repository.Tables.V1

data AesonException = AesonException {file :: !FilePath, errMsg :: !String} deriving (Show)

instance Exception AesonException

rewriteCreatedAt :: AsValue t => t -> t
rewriteCreatedAt =
  _Array % mapped % AO.key "createdAt" % _String
    %~ \t -> let [a, b] = words t in a <> "T" <> b <> "Z"

rewriteTime :: AsValue t => t -> t
rewriteTime =
  _Array % mapped % AO.key "time" % _String
    %~ \t -> let [a, b] = words t in a <> "T" <> b <> "Z"

rewriteDayOfTheWeek :: AsValue t => t -> t
rewriteDayOfTheWeek =
  _Array % mapped % AO.key "dayOfTheWeek" % _String
    %~ \t -> let [_, b] = words t in b

insertFromFile ::
  forall a m.
  (FromJSON a, MonadSelda m, Relational a, Show a) =>
  Table a ->
  FilePath ->
  m (Either AesonException ())
insertFromFile t fp = do
  eVal <- liftIO $ eitherDecodeFileStrict' @Value fp
  let parsed = do
        val <- eVal
        case fromJSON @[a] $ val & rewriteCreatedAt & rewriteTime of
          Error err -> Left err
          Success v -> Right v
  first (AesonException fp) parsed `forM` \os -> do
    print @Text $ "parsed " <> show os
    insert_ t os

insertOwnersFromFile :: MonadSelda m => FilePath -> m (Either AesonException ())
insertOwnersFromFile = insertFromFile @OwnerSQL ownersTable

insertPetsFromFile :: MonadSelda m => FilePath -> m (Either AesonException ())
insertPetsFromFile = insertFromFile @PetSQL petsTable

insertActsFromFile :: MonadSelda m => FilePath -> m (Either AesonException ())
insertActsFromFile = insertFromFile @ActSQL actsTable

insertPricesFromFile :: (MonadSelda m, MonadCatch m) => FilePath -> m (Either AesonException ())
insertPricesFromFile fp = do
  eVal <- liftIO $ eitherDecodeFileStrict' @Value fp
  let parsed = do
        val <- eVal
        case fromJSON @[PriceSQL] $ rewriteCreatedAt val of
          Error err -> Left err
          Success v -> Right v
  parsed `forM_` tryInsert pricesTable
  first (AesonException fp) parsed
    `forM` mapM_
      ( \p ->
          update_
            pricesTable
            (\r -> r ! #priceType .== literal (p ^. #priceType))
            (`with` [#price := literal (p ^. #price)])
      )

insertWorkHoursFromFile ::
  (MonadCatch m, MonadSelda m) => FilePath -> m (Either AesonException ())
insertWorkHoursFromFile fp = do
  eVal <- liftIO $ eitherDecodeFileStrict' @Value fp
  let parsed = do
        val <- eVal
        case fromJSON @[WorkHoursSQL] $ val & rewriteCreatedAt & rewriteDayOfTheWeek of
          Error err -> Left err
          Success v -> Right v
  parsed `forM_` tryInsert workHoursTable
  first (AesonException fp) parsed
    `forM` mapM_
      ( \wh ->
          update_
            workHoursTable
            (\r -> r ! #dayOfTheWeek .== literal (wh ^. #dayOfTheWeek))
            (`with` [#start := literal (wh ^. #start), #end := literal (wh ^. #end)])
      )

insertTreatmentsFromFile :: MonadSelda m => FilePath -> m (Either AesonException ())
insertTreatmentsFromFile = insertFromFile @TreatmentSQL treatmentsTable

insertConsultationsFromFile :: MonadSelda m => FilePath -> m (Either AesonException ())
insertConsultationsFromFile = insertFromFile @ConsultationSQL consultationsTable

data DataFiles = DataFiles
  { dbUrl :: !Text
  , owners :: !FilePath
  , pets :: !FilePath
  , acts :: !FilePath
  , prices :: !FilePath
  , workHours :: !FilePath
  , treatments :: !FilePath
  , consultations :: !FilePath
  }
  deriving (Show, Generic)

instance ParseRecord DataFiles

insertDataFromFiles :: (MonadSelda m, MonadMask m) => DataFiles -> m ()
insertDataFromFiles
  DataFiles
    { owners = owners'
    , pets = pets'
    , acts = acts'
    , prices = prices'
    , workHours = workHours'
    , treatments = treatments'
    , consultations = consultations'
    } = transaction $ do
    mapM_
      runThrow
      [ insertOwnersFromFile owners'
      , insertPetsFromFile pets'
      , insertActsFromFile acts'
      , insertPricesFromFile prices'
      , insertWorkHoursFromFile workHours'
      , insertConsultationsFromFile consultations'
      , insertTreatmentsFromFile treatments'
      ]
   where
    runThrow :: (MonadIO m) => m (Either AesonException a) -> m ()
    runThrow a = void $ either (liftIO . CE.throwIO) pure =<< a

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

main :: IO ()
main = do
  cfg <- getRecord @_ @DataFiles "Migrator config"
  withPostgreSQL
    (uriToConnectInfo (dbUrl cfg) ?: error "could not parse DB URL")
    ( do
        migrateUp
        pwd <- liftIO $ hashPasswordUsingPolicy fastBcryptHashingPolicy "changeme"
        insert_ passwordTable [PasswordSQL{key = toId 1, password = pwd ?: ""}]
        insertDataFromFiles cfg
    )
