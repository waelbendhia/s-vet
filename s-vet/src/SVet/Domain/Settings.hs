module SVet.Domain.Settings (
  PriceType (..),
  Prices,
  Schedule,
  WorkHours (..),
  Settings (..),
  WithField (..),
  WeekDay (..),
) where

import Data.Aeson
import Data.Aeson.TH
import Data.Aeson.Types
import Data.Time
import Database.Selda hiding (toString)
import Optics
import Relude
import SVet.Domain.Internal
import Servant

data PriceType = ConsultationPrice | EmergencyPrice
  deriving (Show, Read, Bounded, Enum, Eq, Generic, Ord)

deriveJSON defaultOptions ''PriceType

instance FromHttpApiData PriceType where
  parseUrlPiece "consultation" = Right ConsultationPrice
  parseUrlPiece "emergency" = Right EmergencyPrice
  parseUrlPiece t = Left $ "invalid price type " <> t

instance ToJSONKey PriceType where
  toJSONKey = toJSONKeyText \case
    ConsultationPrice -> "consultation"
    EmergencyPrice -> "emergency"

instance FromJSONKey PriceType where
  fromJSONKey =
    FromJSONKeyTextParser (either (fail . toString) pure . parseUrlPiece)

instance SqlType PriceType

type Prices = Map PriceType Int

data WorkHours = WorkHours {start :: !TimeOfDay, end :: !TimeOfDay}
  deriving (Show, Read, Eq, Generic)

makeFieldLabelsWith noPrefixFieldLabels ''WorkHours

deriveJSON defaultOptions ''WorkHours

newtype WeekDay = WeekDay DayOfWeek
  deriving
    (Show, Read, Eq, Ord, FromJSONKey, ToJSONKey, FromJSON, ToJSON, Enum)

instance SqlType WeekDay

instance Bounded WeekDay where
  minBound = WeekDay Monday

  maxBound = WeekDay Sunday

type Schedule = Map WeekDay WorkHours

data Settings = Settings {prices :: Prices, schedule :: Schedule}
  deriving (Show, Eq, Read, Generic)

makeFieldLabelsWith noPrefixFieldLabels ''Settings

deriveJSON defaultOptions ''Settings
