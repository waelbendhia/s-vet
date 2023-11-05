{-# LANGUAGE BlockArguments #-}

module SVet.Repository.Statistics (
  Statistics (..),
  runStatistics,
  StatisticsError (..),
  getConsultationsByHourStatistics,
  getConsultationsStatistics,
) where

import Data.Time
import Database.Selda
import Polysemy
import Polysemy.Error
import Relude
import Relude.Extra
import SVet.Domain
import SVet.Repository.Tables

data Statistics m a where
  GetConsultationsByHourStatistics :: Email -> Day -> Day -> Statistics m ConsultationsByHour
  GetConsultationsStatistics :: Email -> UTCTime -> UTCTime -> Statistics m ConsultationStatistics

makeSem ''Statistics

runStatistics ::
  (MonadSelda (Sem r), Members '[Error StatisticsError] r) =>
  Sem (Statistics : r) a ->
  Sem r a
runStatistics = interpret \case
  GetConsultationsByHourStatistics v s e -> do
    ts <- query do
      c <- select consultationsTable `suchThat` (not_ . (! #cancelled))
      restrictByVet v (c ! #petKey)
      let t = c ! #time
          toUTCTime d = literal . UTCTime d . secondsToDiffTime
      restrict (toUTCTime s 0 .<= t .&& t .<= toUTCTime e 86400)
      pure $ t :*: ifNull (literal 0) (c ! #paid)
    pure $
      foldr
        ( \(t :*: a) ->
            insertWith (<>) (utcTimeToDayHour t) (HourSummary 1 a)
        )
        mempty
        ts
  GetConsultationsStatistics v after' before' -> do
    (totalCount :*: paidCount :*: revenue') <-
      throwIfNoRows (SEStatisticsNoRows v after' before') $ aggregate do
        c <- selectConsultationsBetween after' before'
        restrictByVet v (c ! #petKey)
        let revenue' = sum_ (ifNull (literal 0) (c ! #paid))
            totalCount = count (c ! #key)
            paidCount =
              sum_ $
                ifThenElse
                  (isNull $ c ! #paid)
                  (literal @Int 1)
                  (literal @Int 0)
        pure $ totalCount :*: paidCount :*: revenue'
    pure $
      ConsultationStatistics
        (totalCount - paidCount)
        paidCount
        revenue'

selectConsultationsBetween ::
  UTCTime -> UTCTime -> Query s (Row s ConsultationSQL)
selectConsultationsBetween after' before' =
  select consultationsTable
    `suchThat` \c ->
      c ! #time .>= literal after'
        .&& c ! #time .<= literal before'
        .&& not_ (c ! #cancelled)

restrictByVet :: Email -> Col t (ID PetSQL) -> Query t ()
restrictByVet v pk = do
  p <- innerJoin (\p -> p ! #key .== pk) $ select petsTable
  void $
    innerJoin (\o -> o ! #key .== p ! #ownerKey) $
      select ownersTable `suchThat` #vet `is` v

data StatisticsError = SEStatisticsNoRows !Email !UTCTime !UTCTime deriving (Show)
