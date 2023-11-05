{-# LANGUAGE BlockArguments #-}

module SVet.Repository.Owner (
  OwnerRepository (..),
  OwnerSearchRequest (..),
  OwnerError (..),
  insertOwner,
  updateOwner,
  getOwner,
  searchOwners,
  createOwnerPassword,
  getOwnerPassword,
  runOwnerRepository,
) where

import Database.Selda hiding (toText)
import Optics
import Polysemy
import Polysemy.Error
import Polysemy.Input
import Relude hiding (Reader, ask, asks)
import SVet.Domain
import SVet.Repository.Crypt
import SVet.Repository.Tables

data OwnerRepository m a where
  InsertOwner :: Email -> Owner -> OwnerRepository m (Entity Owner)
  UpdateOwner :: Email -> EntKey Owner -> Owner -> OwnerRepository m (Entity Owner)
  GetOwner :: Email -> EntKey Owner -> OwnerRepository m OwnerWithPetsAndBalance
  SearchOwners :: Email -> OwnerSearchRequest -> OwnerRepository m (SearchResult (Entity Owner))
  CreateOwnerPassword :: Email -> EntKey Owner -> OwnerRepository m Text
  GetOwnerPassword :: EntKey Owner -> OwnerRepository m (Maybe ByteString)

makeSem ''OwnerRepository

runOwnerRepository ::
  (MonadSelda (Sem r), Members '[Input UTCTime, Error OwnerError, Input Int, Crypt] r) =>
  Sem (OwnerRepository : r) a ->
  Sem r a
runOwnerRepository = interpret \case
  InsertOwner v o -> do
    now <- input
    k <-
      insertWithPK
        ownersTable
        [ OwnerSQL
            { key = def
            , vet = v
            , name = o ^. #name
            , email = o ^. #email
            , phonenumber = o ^. #phonenumber
            , address = o ^. #address
            , createdAt = now
            }
        ]
    pure $ WithField (fromSeldaID k) o
  UpdateOwner v k o ->
    WithField k o
      <$ update_
        ownersTable
        ((.&&) <$> #key `is` toSeldaID k <*> #vet `is` v)
        ( `with`
            [ #name := literal (o ^. #name)
            , #email := literal (o ^. #email)
            , #phonenumber := literal (o ^. #phonenumber)
            , #address := literal (o ^. #address)
            ]
        )
  GetOwner v k -> do
    r <- getOwners (#key `is` toSeldaID k <.&&> #vet `is` v)
    o <- justOne (OEOwnerNotFound v k) r
    ps <- asEntity <<$>> query (select petsTable `suchThat` #ownerKey `is` toSeldaID k)
    res <- query $ aggregate do
      p <- select petsTable `suchThat` #ownerKey `is` toSeldaID k
      cs <- select consultationsTable `suchThat` (not_ . (! #cancelled))
      restrict $ (cs ! #petKey) .== (p ! #key)
      let sum' val = sum_ (ifNull (literal 0) val)
      pure (sum' (cs ! #paid) :*: sum' (cs ! #remaining))
    case res of
      [paid' :*: unpaid] -> pure $ WithField paid' $ WithField unpaid $ WithField ps o
      _ -> throw $ OEOwnerNotFound v k
  SearchOwners v r ->
    over (mapped % #rows % mapped) asEntity $
      countAndLimit (r ^. #page) (r ^. #itemsPerPage) (! #key) do
        o <- select ownersTable `suchThat` #vet `is` v
        forM_ (r ^. #search) $ restrict . ilike (o ! #name)
        pure o
  CreateOwnerPassword v okey -> do
    k <-
      throwIfNoRows (OEOwnerNotFound v okey) $
        #key
          `from` select ownersTable
          `suchThat` (#vet `is` v <.&&> #key `is` toSeldaID okey)
    secret <- inputs toPassword
    now <- input
    hashed <- hash secret
    _ <-
      insert
        ownerPasswordsTable
        [ OwnerPasswordSQL
            { ownerKey = k
            , password = hashed
            , createdAt = now
            }
        ]
    pure secret
   where
    toPassword =
      toText
        . (\x -> replicate (6 - length x) '0' <> x)
        . show @String
        . (`div` (10 :: Int) ^ (6 :: Int))
        . abs
  GetOwnerPassword okey ->
    listToMaybe
      <$> query
        (#password `from` select ownerPasswordsTable `suchThat` (#ownerKey `is` toSeldaID okey))

getOwners ::
  (MonadSelda f) =>
  (Row (Inner (Backend f)) OwnerSQL -> Col (Inner (Backend f)) Bool) ->
  f [Entity Owner]
getOwners p = asEntity <<$>> query (select ownersTable `suchThat` p)

data OwnerError = OEOwnerNotFound !Email !(EntKey Owner) deriving (Show)
