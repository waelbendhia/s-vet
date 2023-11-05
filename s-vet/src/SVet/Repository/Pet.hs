{-# LANGUAGE BlockArguments #-}

module SVet.Repository.Pet (
  PetRepository (..),
  PetError (..),
  runPetRepository,
  insertPet,
  updatePet,
  getPetsByOwner,
  searchPets,
  getPet,
) where

import Data.Aeson
import Database.Selda
import Optics
import Polysemy
import Polysemy.Error
import Polysemy.Input
import Relude hiding (Reader, ask, first)
import SVet.Domain
import SVet.Repository.Internal
import SVet.Repository.Tables

data PetError
  = PEPetNotFound !(EntKey Pet)
  | PEOwnerNotFound !Email !(EntKey Owner)
  deriving (Show)

fromFullRow :: (PetSQL :*: OwnerSQL) -> PetWithOwner
fromFullRow (p :*: o) = WithField (asEntity o) (asEntity p)

searchPetsWithOwnerRestriction ::
  (MonadSelda m) =>
  ( Row (Inner (Inner (Inner (Backend m)))) OwnerSQL ->
    Col (Inner (Inner (Inner (Backend m)))) Bool
  ) ->
  PetSearchRequest ->
  m (SearchResult PetWithOwner)
searchPetsWithOwnerRestriction pred' r = over (mapped % #rows % mapped) fromFullRow
  . countAndLimit (r ^. #page) (r ^. #itemsPerPage) ((! #key) . first)
  $ do
    ps <- select petsTable
    os <-
      innerJoin
        (\o -> ps ! #ownerKey .== o ! #key)
        (select ownersTable `suchThat` pred')
    (r ^. #search) `forM_` \s ->
      restrict $
        (ps ! #name `ilike` s)
          .|| (ps ! #breed `ilike` s)
          .|| (os ! #name `ilike` s)
    pure (ps :*: os)

getPetWithOwnerRestriction ::
  (MonadSelda (Sem r), Members '[Error PetError] r) =>
  ( Row (Inner (Inner (Backend (Sem r)))) OwnerSQL ->
    Col (Inner (Inner (Backend (Sem r)))) Bool
  ) ->
  EntKey Pet ->
  Sem r (WithField l [Entity Consultation] PetWithOwner)
getPetWithOwnerRestriction pred' k = do
  ps <- query $ do
    ps <- select petsTable `suchThat` (#key `is` toSeldaID k)
    os <-
      innerJoin
        (\o -> ps ! #ownerKey .== o ! #key)
        (select ownersTable `suchThat` pred')
    pure (ps :*: os)
  p <-
    maybe (throw $ PEPetNotFound k) pure . listToMaybe $
      fromFullRow <$> ps
  cs <- asEntity <<$>> query (limit 0 5 $ getConsultationsByPetQuery (p ^. #key))
  pure $ WithField cs p

data PetRepository m a where
  InsertPet :: Email -> EntKey Owner -> Pet -> PetRepository m (Entity Pet)
  UpdatePet :: Email -> EntKey Pet -> Pet -> PetRepository m (Entity Pet)
  GetPetsByOwner :: Email -> EntKey Owner -> PetRepository m [Entity Pet]
  SearchPets ::
    Either (EntKey Owner) Email ->
    PetSearchRequest ->
    PetRepository m (SearchResult PetWithOwner)
  GetPet :: Either (EntKey Owner) Email -> EntKey Pet -> PetRepository m PetWithConsultations

makeSem ''PetRepository

runPetRepository ::
  (MonadSelda (Sem r), Members '[Input UTCTime, Error PetError] r) =>
  Sem (PetRepository : r) a ->
  Sem r a
runPetRepository = interpret \case
  InsertPet v ok pet -> do
    now <- input
    ok' <-
      throwIfNoRows (PEOwnerNotFound v ok) $
        restrictByVet v (literal $ toSeldaID ok)
    k <-
      insertWithPK
        petsTable
        [ PetSQL
            { key = def
            , ownerKey = ok'
            , name = pet ^. #name
            , age = pet ^. #age
            , species = pet ^. #species
            , sex = pet ^. #sex
            , breed = pet ^. #breed
            , neutered = pet ^. #neutered
            , bloodType = pet ^. #bloodType
            , allergies = decodeUtf8 $ encode $ pet ^. #allergies
            , createdAt = now
            }
        ]
    pure $ WithField (fromSeldaID k) pet
  GetPetsByOwner v ok -> getPets v (#ownerKey `is` toSeldaID ok)
  SearchPets acc r ->
    searchPetsWithOwnerRestriction (either (is #key . toSeldaID) (is #vet) acc) r
  UpdatePet v k p -> do
    _ <- throwIfNoRows (PEPetNotFound k) $ do
      ps <- select petsTable `suchThat` #key `is` toSeldaID k
      _ <-
        innerJoin (\o -> o ! #key .== ps ! #ownerKey) $
          select ownersTable `suchThat` #vet `is` v
      pure $ ps ! #key
    WithField k p
      <$ update_
        petsTable
        (#key `is` toSeldaID k)
        ( `with`
            [ #name := literal (p ^. #name)
            , #age := literal (p ^. #age)
            , #species := literal (p ^. #species)
            , #breed := literal (p ^. #breed)
            , #neutered := literal (p ^. #neutered)
            , #bloodType := literal (p ^. #bloodType)
            , #allergies := literal (decodeUtf8 $ encode $ p ^. #allergies)
            ]
        )
  GetPet acc k -> getPetWithOwnerRestriction (either (is #key . toSeldaID) (is #vet) acc) k

getPets ::
  (MonadSelda f) =>
  Email ->
  (Row (Inner (Backend f)) PetSQL -> Col (Inner (Backend f)) Bool) ->
  f [Entity Pet]
getPets v p =
  asEntity <<$>> query do
    ps <- select petsTable `suchThat` p
    _ <-
      innerJoin (\o -> o ! #key .== ps ! #ownerKey) $
        select ownersTable `suchThat` #vet `is` v
    pure ps

restrictByVet :: Email -> Col (Inner s) (ID OwnerSQL) -> Query s (Col s (ID OwnerSQL))
restrictByVet v k =
  #key
    `from` select ownersTable
    `suchThat` (\o -> (o & #vet `is` v) .&& o ! #key .== k)
