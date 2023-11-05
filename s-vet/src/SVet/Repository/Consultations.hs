{-# LANGUAGE BlockArguments #-}

module SVet.Repository.Consultations (
  ConsultationRepository (..),
  ConsultationError (..),
  runConsultationRepository,
  insertConsultation,
  updateConsultation,
  getConsultation,
  getConsultationWithPrevious,
  searchConsultations,
  getConsultationsByPet,
  deleteConsultation,
) where

import qualified Data.Map.Strict as Map
import Database.Selda
import Optics
import Polysemy
import Polysemy.Error
import Polysemy.Input
import Relude hiding (first)
import SVet.Domain
import SVet.Repository.Internal
import SVet.Repository.Tables
import SVet.Repository.Treatment

restrictByVet :: Email -> Col s (ID PetSQL) -> Query s (Col s (ID PetSQL))
restrictByVet v pk = do
  ok <- #key `from` select ownersTable `suchThat` #vet `is` v
  p <- select petsTable
  restrict (p ! #ownerKey .== ok .&& p ! #key .== pk)
  pure $ p ! #key

data ConsultationError
  = CEPetNotFound !Email !(EntKey Pet)
  | CEConsultationNotFound !(EntKey Consultation)
  deriving (Show)

selectOwnedPet ::
  (MonadSelda (Sem r), Members '[Error ConsultationError] r) =>
  Email ->
  EntKey Pet ->
  Sem r (ID PetSQL)
selectOwnedPet v pk =
  throwIfNoRows (CEPetNotFound v pk) $ restrictByVet v (literal $ toSeldaID pk)

selectOwnedConsultation ::
  (MonadSelda (Sem r), Members '[Error ConsultationError] r) =>
  Email ->
  EntKey Consultation ->
  Sem r (ID ConsultationSQL)
selectOwnedConsultation v ck = throwIfNoRows (CEConsultationNotFound ck) do
  c <- select consultationsTable `suchThat` #key `is` toSeldaID ck
  _ <- restrictByVet v (c ! #petKey)
  pure $ c ! #key

data ConsultationRepository m a where
  InsertConsultation ::
    Email ->
    EntKey Pet ->
    Consultation ->
    ConsultationRepository m (Entity Consultation)
  UpdateConsultation :: Email -> Entity Consultation -> ConsultationRepository m (Entity Consultation)
  GetConsultation ::
    Either (EntKey Owner) Email ->
    EntKey Consultation ->
    ConsultationRepository m ConsultationWithPet
  GetConsultationWithPrevious ::
    Either (EntKey Owner) Email ->
    EntKey Consultation ->
    ConsultationRepository m ConsultationWithPrevious
  SearchConsultations ::
    Either (EntKey Owner) Email ->
    ConsultationSearchRequest ->
    ConsultationRepository m (SearchResult ConsultationWithPet)
  GetConsultationsByPet ::
    Email ->
    EntKey Pet ->
    Int ->
    ConsultationRepository m [Entity Consultation]
  DeleteConsultation ::
    Email ->
    EntKey Consultation ->
    ConsultationRepository m ()

makeSem ''ConsultationRepository

runConsultationRepository ::
  (MonadSelda (Sem r), Members '[Input UTCTime, Error ConsultationError] r) =>
  Sem (ConsultationRepository : r) a ->
  Sem r a
runConsultationRepository = interpret \case
  InsertConsultation v pKey c -> do
    now <- input
    pk <- selectOwnedPet v pKey
    k <-
      insertWithPK
        consultationsTable
        [ ConsultationSQL
            { key = def
            , petKey = pk
            , weight = c ^. #weight
            , time = c ^. #time
            , motive = c ^. #motive
            , diagnosis = c ^. #diagnosis
            , total = c ^? #amount % _Just % #total
            , paid = c ^? #amount % _Just % #paid
            , remaining = c ^? #amount % _Just % #remaining
            , cancelled = False
            , createdAt = now
            }
        ]
    insertTreatments now k (c ^. #treatment)
    ts <- getTreatments k
    pure $ WithField (fromSeldaID k) $ c & #treatment .~ ts
  UpdateConsultation v c -> do
    now <- input
    ck <- throwIfNoRows (CEConsultationNotFound (c ^. #key)) do
      cr <- select consultationsTable `suchThat` #key `is` toSeldaID (c ^. #key)
      _ <- restrictByVet v (cr ! #petKey)
      pure $ cr ! #key
    update_
      consultationsTable
      (#key `is` ck)
      ( `with`
          [ #weight := literal (c ^. #weight)
          , #time := literal (c ^. #time)
          , #motive := literal (c ^. #motive)
          , #diagnosis := literal (c ^. #diagnosis)
          , #total := literal (c ^? #amount % _Just % #total)
          , #paid := literal (c ^? #amount % _Just % #paid)
          , #remaining := literal (c ^? #amount % _Just % #remaining)
          ]
      )
    deleteFrom_ treatmentsTable (#consultationKey `is` ck)
    insertTreatments now ck (c ^. #treatment)
    pure c
  GetConsultation acc k -> getConsultation_ acc k
  GetConsultationWithPrevious acc k -> do
    c <- getConsultation_ acc k
    let petK = toSeldaID $ c ^. #pet % #key
    cs <- query $ limit 0 3 do
      cs <- select consultationsTable
      restrict $ cs ! #petKey .== literal petK
      restrict $ not_ (cs ! #cancelled)
      restrict $ (cs ! #time) .< literal (c ^. #time)
      order (cs ! #time) Desc
      pure cs
    pure $ WithField (asEntity <$> cs) c
  SearchConsultations acc r -> searchConsultationsG (either (is #key . toSeldaID) (is #vet) acc) r
  DeleteConsultation v cKey -> do
    ck <- selectOwnedConsultation v cKey
    update_
      consultationsTable
      (#key `is` ck)
      (`with` [#cancelled := true])
  GetConsultationsByPet v pKey lmt -> do
    rows' <- query $ limit 0 lmt do
      c <- getConsultationsByPetQuery pKey
      _ <- restrictByVet v (c ! #petKey)
      pure c
    let cs = asEntity <$> rows'
    if null cs
      then pure []
      else do
        ts <-
          fmap asTreatmentLookup . query . getTreatmentsQuery $ \c ->
            c ! #consultationKey `isIn` (cs <&> literal . toSeldaID . view #key)
        pure $ cs <&> \c -> c & #treatment .~ maybeToMonoid (ts ^. at (c ^. #key))
   where
    asTreatmentLookup ((consultationID :*: r) : ts) =
      asTreatmentLookup ts
        & Map.insertWith (<>) (fromSeldaID consultationID) (one $ rowToTreatment r)
    asTreatmentLookup [] = mempty @(Map (EntKey Consultation) [AppliedTreatment])
 where
  searchConsultationsG pred' r =
    over (mapped % #rows % mapped) fullFromRow $
      countAndLimit (r ^. #page) (r ^. #itemsPerPage) ((! #key) . first) do
        row'@(c :*: p :*: o) <- selectFullRow pred'
        restrict (p ! #key .== c ! #petKey)
        (r ^. #after) `forM_` \a -> restrict (c ! #time .>= literal a)
        (r ^. #before) `forM_` \b -> restrict (c ! #time .<= literal b)
        (r ^. #term) `forM_` \t ->
          restrict $ (o ! #name `ilike` t) .|| (p ! #name `ilike` t) .|| (p ! #breed `ilike` t)
        (r ^. #status) `forM_` \case
          Pending -> restrict (c ! #paid .< c ! #total)
          Completed -> restrict (c ! #paid .>= c ! #total)
        order (c ! #time) Desc
        pure row'
  getConsultation_ acc k = do
    c <-
      throwIfNoRows (CEConsultationNotFound k) $
        selectFullRow
          (either (is #key . toSeldaID) (is #vet) acc)
          `suchThat` ((#key `is` toSeldaID k) . first)
    ts <- getTreatments $ toSeldaID k
    pure $ fullFromRow c & #treatment .~ ts

getTreatmentsQuery ::
  (Row (Inner s) TreatmentSQL -> Col (Inner s) Bool) ->
  Query
    s
    (Col s (ID ConsultationSQL) :*: Col s Int :*: Col s (Maybe Text) :*: Row s (Maybe ActSQL))
getTreatmentsQuery p = do
  r <- select treatmentsTable `suchThat` p
  a <- leftJoin (\a -> just (a ! #key) .== r ! #actKey) (select actsTable)
  pure $ r ! #consultationKey :*: r ! #quantity :*: r ! #general :*: a

getTreatments :: (MonadSelda m) => ID ConsultationSQL -> m [AppliedTreatment]
getTreatments k =
  (\(_ :*: r) -> rowToTreatment r) <<$>> query (getTreatmentsQuery $ #consultationKey `is` k)

rowToTreatment :: (Int :*: Maybe Text :*: Maybe ActSQL) -> AppliedTreatment
rowToTreatment (q :*: gen :*: act) = AppliedTreatment q $ case (gen, act) of
  (Just _, Just _) -> error "treatment row is both general and specific"
  (Nothing, Nothing) -> error "treatment row is neither general nor specific"
  (Just g, Nothing) -> General g
  (_, Just a) -> Specific $ WithField (fromSeldaID $ a ^. #key) $ Act (a ^. #price) (a ^. #name)

fullFromRow ::
  (ConsultationSQL :*: (PetSQL :*: OwnerSQL)) ->
  ConsultationWithPet
fullFromRow (c :*: p :*: o) =
  WithField (WithField (asEntity o) (asEntity p)) (asEntity c)

selectFullRow ::
  ( Row (Inner (Inner t)) OwnerSQL ->
    Col (Inner (Inner t)) Bool
  ) ->
  Query t (Row t ConsultationSQL :*: (Row t PetSQL :*: Row t OwnerSQL))
selectFullRow pred' = do
  c <- select consultationsTable `suchThat` (not_ . (! #cancelled))
  p <- innerJoin ((c ! #petKey .==) . (! #key)) $ select petsTable
  o <- innerJoin ((p ! #ownerKey .==) . (! #key)) $ select ownersTable `suchThat` pred'
  pure $ c :*: p :*: o

insertTreatments ::
  (MonadSelda m) =>
  UTCTime ->
  ID ConsultationSQL ->
  [AppliedTreatment] ->
  m ()
insertTreatments now cKey ts = insert_ treatmentsTable $ treatmentToRow now cKey <$> ts
