{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE EmptyCase #-}

module SVet.Repository.Act (
  ActRepository (..),
  runActRepository,
  insertAct,
  updateAct,
  getAct,
  deleteAct,
  searchActs,
  ActError (..),
) where

import Database.Selda
import Optics
import Polysemy
import Polysemy.Error
import Polysemy.Input
import Relude hiding (Reader, ask)
import SVet.Domain
import SVet.Repository.Tables
import SVet.Selda

fromRow :: ActSQL -> Entity Act
fromRow ActSQL{key = k, price = p, name = n} = WithField (fromSeldaID k) $ Act p n

getActs ::
  (MonadSelda f) =>
  (Row (Inner (Backend f)) ActSQL -> Col (Inner (Backend f)) Bool) ->
  f [Entity Act]
getActs p = fromRow <<$>> query (select actsTable `suchThat` p)

data ActError = AEActNotFound !Email !(EntKey Act) deriving (Show)

data ActRepository m a where
  InsertAct :: Email -> Act -> ActRepository m (Entity Act)
  UpdateAct :: Email -> Entity Act -> ActRepository m (Entity Act)
  GetAct :: Email -> EntKey Act -> ActRepository m (Entity Act)
  DeleteAct :: Email -> EntKey Act -> ActRepository m ()
  SearchActs :: Email -> Maybe Text -> ActRepository m [Entity Act]

makeSem ''ActRepository

runActRepository ::
  (Members '[Embed IO, Input UTCTime, Error ActError, Selda] r) =>
  Sem (ActRepository : r) a ->
  Sem r a
runActRepository = interpret \case
  InsertAct vet' a -> do
    now <- input
    k <-
      insertWithPK
        actsTable
        [ ActSQL
            { key = def
            , vet = vet'
            , price = a ^. #price
            , name = a ^. #name
            , createdAt = now
            }
        ]
    pure $ WithField (fromSeldaID k) a
  UpdateAct vet' a -> do
    update_
      actsTable
      (#key `is` toSeldaID (a ^. #key) <.&&> #vet `is` vet')
      (`with` [#price := literal (a ^. #price), #name := literal (a ^. #name)])
    pure a
  GetAct vet' k -> do
    r <- getActs (#key `is` toSeldaID k <.&&> #vet `is` vet')
    justOne (AEActNotFound vet' k) r
  DeleteAct vet' k -> do
    ks <-
      query $
        #key
          `from` select actsTable
          `suchThat` ((.&&) <$> #key `is` toSeldaID k <*> #vet `is` vet')
    ks `forM_` \ak -> do
      deleteFrom_ treatmentsTable (#actKey `is` Just ak)
      deleteFrom_ actsTable (#key `is` ak)
  SearchActs vet' t ->
    fromRow <<$>> query do
      a <- select actsTable `suchThat` #vet `is` vet'
      mapM_ (restrict . ilike (a ! #name)) t
      order (a ! #name) Asc
      pure a
