{-# LANGUAGE BlockArguments #-}

module SVet.Repository.Treatment (
  TreatmentRepository (..),
  treatmentToRow,
  TreatmentError (..),
  insertTreatment,
  deleteTreatment,
  getTreatmentsByConsultation,
  runTreatmentRepository,
) where

import Database.Selda
import Optics
import Polysemy
import Polysemy.Error
import Polysemy.Input
import Relude
import SVet.Domain
import SVet.Repository.Tables

data TreatmentError = TEConsultationNotFound !Email !(EntKey Consultation) deriving (Show)

selectOwnedConsultation ::
  (MonadSelda (Sem r), Members '[Error TreatmentError] r) =>
  Email ->
  EntKey Consultation ->
  Sem r (ID ConsultationSQL)
selectOwnedConsultation vet' ck = throwIfNoRows (TEConsultationNotFound vet' ck) do
  c <- select consultationsTable `suchThat` #key `is` toSeldaID ck
  p <- innerJoin (\p -> p ! #key .== c ! #petKey) (select petsTable)
  _ <- innerJoin (\o -> p ! #ownerKey .== o ! #key .&& (o & #vet `is` vet')) (select ownersTable)
  pure $ c ! #key

data TreatmentRepository m a where
  InsertTreatment ::
    Email ->
    EntKey Consultation ->
    AppliedTreatment ->
    TreatmentRepository m AppliedTreatment
  DeleteTreatment ::
    Email ->
    EntKey Consultation ->
    AppliedTreatmentKey ->
    TreatmentRepository m ()
  GetTreatmentsByConsultation ::
    Email ->
    EntKey Consultation ->
    TreatmentRepository m [AppliedTreatment]

makeSem ''TreatmentRepository

runTreatmentRepository ::
  (MonadSelda (Sem r), Members '[Input UTCTime, Error TreatmentError] r) =>
  Sem (TreatmentRepository : r) a ->
  Sem r a
runTreatmentRepository = interpret \case
  InsertTreatment v cKey t -> do
    now <- input
    i <- queryInto treatmentsTable do
      c <- select consultationsTable `suchThat` #key `is` toSeldaID cKey
      p <- innerJoin (\p -> p ! #key .== c ! #petKey) (select petsTable)
      _ <-
        innerJoin
          (\o -> p ! #ownerKey .== o ! #key .&& (o & #vet `is` v))
          (select ownersTable)
      pure $
        row
          TreatmentSQL
            { consultationKey = toSeldaID cKey
            , quantity = t ^. #quantity
            , general = t ^? #treatment % _General
            , actKey = toSeldaID <$> t ^? #treatment % _Specific % #key
            , createdAt = now
            }
    when (i == 0) (throw $ TEConsultationNotFound v cKey)
    pure t
  DeleteTreatment v cKey t -> do
    ck <- selectOwnedConsultation v cKey
    deleteFrom_ treatmentsTable $ \r ->
      let matchTreatment = case t of
            Specific k -> r & #actKey `is` Just (toSeldaID k)
            General k -> r & #general `is` Just k
          matchConsultation = r & #consultationKey `is` ck
       in matchConsultation .&& matchTreatment
  GetTreatmentsByConsultation v cKey ->
    rowToTreatment <<$>> query do
      ck <- ownedConsultation
      r <- select treatmentsTable
      restrict $ r ! #consultationKey .== ck
      a <- leftJoin (\a -> just (a ! #key) .== r ! #actKey) (select actsTable)
      pure $ r ! #quantity :*: r ! #general :*: a
   where
    ownedConsultation = do
      c <- select consultationsTable `suchThat` #key `is` toSeldaID cKey
      p <- innerJoin (\p -> p ! #key .== c ! #petKey) $ select petsTable
      _ <-
        innerJoin (\o -> o ! #key .== p ! #ownerKey) $
          select ownersTable `suchThat` #vet `is` v
      pure $ c ! #key

rowToTreatment :: (Int :*: Maybe Text :*: Maybe ActSQL) -> AppliedTreatment
rowToTreatment (q :*: gen :*: act) = AppliedTreatment q $ case (gen, act) of
  (Just _, Just _) -> error "treatment row is both general and specific"
  (Nothing, Nothing) -> error "treatment row is neither general nor specific"
  (Just g, Nothing) -> General g
  (_, Just a) -> Specific $ WithField (fromSeldaID $ a ^. #key) $ Act (a ^. #price) (a ^. #name)

treatmentToRow ::
  UTCTime -> ID ConsultationSQL -> AppliedTreatment -> TreatmentSQL
treatmentToRow now cKey t =
  TreatmentSQL
    { consultationKey = cKey
    , quantity = t ^. #quantity
    , general = t ^? #treatment % _General
    , actKey = toSeldaID <$> t ^? #treatment % _Specific % #key
    , createdAt = now
    }
