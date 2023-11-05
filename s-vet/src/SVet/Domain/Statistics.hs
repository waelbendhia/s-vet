module SVet.Domain.Statistics (
  ConsultationStatistics (..),
  DayHour (..),
  utcTimeToDayHour,
  HourSummary (..),
  ConsultationsByHour,
) where

import Data.Aeson
import Data.Aeson.TH
import Data.Aeson.Types
import qualified Data.Text as T
import Data.Time
import Optics
import Relude

data ConsultationStatistics = ConsultationStatistics
  { remainingConsultations :: !Int
  , doneConsultations :: !Int
  , earnings :: !Int
  }
  deriving (Show, Eq)

makeFieldLabelsWith noPrefixFieldLabels ''ConsultationStatistics

deriveJSON defaultOptions ''ConsultationStatistics

newtype DayHour = DayHour (DayOfWeek, Int)
  deriving (Show, Eq, Read, Ord)

utcTimeToDayHour :: UTCTime -> DayHour
utcTimeToDayHour (UTCTime d s) = DayHour (dayOfWeek d, floor s `div` 3600)

dayHourToText :: DayHour -> Text
dayHourToText (DayHour (d, h)) = dow <> "-" <> fromString (show h)
 where
  dow = case toJSON d of
    String v -> v
    _ -> "I shouldn't be here"

instance ToJSON DayHour where
  toJSON = String . dayHourToText

instance ToJSONKey DayHour where
  toJSONKey = toJSONKeyText dayHourToText

dayOfWeekFromText :: MonadFail m => Text -> m DayOfWeek
dayOfWeekFromText t = case T.toLower t of
  "monday" -> pure Monday
  "tuesday" -> pure Tuesday
  "wednesday" -> pure Wednesday
  "thursday" -> pure Thursday
  "friday" -> pure Friday
  "saturday" -> pure Saturday
  "sunday" -> pure Sunday
  _ -> fail "Invalid week day"

dayHourFromText :: MonadFail m => Text -> m DayHour
dayHourFromText s = case T.splitOn "-" s of
  [d, h] -> do
    d' <- dayOfWeekFromText d
    h' <- either fail pure $ eitherDecodeStrict $ encodeUtf8 h
    pure $ DayHour (d', h')
  _ -> fail $ toString $ "invalid format " <> s

instance FromJSON DayHour where
  parseJSON = withText "DayHour" dayHourFromText

instance FromJSONKey DayHour where
  fromJSONKey = FromJSONKeyTextParser dayHourFromText

data HourSummary = HourSummary {numberOfConsultations :: Int, revenue :: Int}
  deriving (Show, Read)

makeFieldLabelsWith noPrefixFieldLabels ''HourSummary

deriveJSON defaultOptions ''HourSummary

instance Semigroup HourSummary where
  HourSummary na ra <> HourSummary nb rb = HourSummary (na + nb) (ra + rb)

instance Monoid HourSummary where mempty = HourSummary 0 0

type ConsultationsByHour = Map DayHour HourSummary
