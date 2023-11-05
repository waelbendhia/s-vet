module SVet.Repository.Tables.V1 where

import Data.Aeson
import Data.Text as T
import Data.Time
import Database.Selda as S
import Database.Selda.JSON ()
import Relude hiding (MonadReader, Reader, ask)
import SVet.Domain

data PasswordSQL = PasswordSQL {key :: ID PasswordSQL, password :: ByteString}
  deriving (Show, Eq, Generic)

instance SqlRow PasswordSQL

passwordTable :: Table PasswordSQL
passwordTable = table "password" [#key :- primary]

data WorkHoursSQL = WorkHoursSQL {start :: !TimeOfDay, end :: !TimeOfDay, dayOfTheWeek :: WeekDay}
  deriving (Show, Eq, Generic)

instance FromJSON WorkHoursSQL

instance SqlRow WorkHoursSQL

workHoursTable :: Table WorkHoursSQL
workHoursTable = table "workhours" [#dayOfTheWeek :- primary]

data PriceSQL = PricesSQL {price :: !Int, priceType :: PriceType}
  deriving (Show, Eq, Generic)

instance FromJSON PriceSQL

instance SqlRow PriceSQL

pricesTable :: Table PriceSQL
pricesTable = table "prices" [#priceType :- primary]

data ActSQL = ActSQL
  { key :: ID ActSQL
  , price :: !Int
  , createdAt :: !UTCTime
  , name :: !Text
  }
  deriving (Show, Eq, Generic)

instance FromJSON ActSQL

instance SqlRow ActSQL

actsTable :: Table ActSQL
actsTable = table "acts" [#key :- autoPrimary]

data OwnerSQL = OwnerSQL
  { key :: ID OwnerSQL
  , createdAt :: !UTCTime
  , name :: !Text
  , email :: !(Maybe Email)
  , phonenumber :: !(Maybe Text)
  , address :: !(Maybe Text)
  }
  deriving (Show, Eq, Generic)

instance FromJSON OwnerSQL

instance SqlRow OwnerSQL

ownersTable :: Table OwnerSQL
ownersTable =
  table
    "owners"
    [#key :- autoPrimary, #email :- unique, #phonenumber :- unique]

data PetSQL = PetSQL
  { key :: ID PetSQL
  , ownerKey :: ID OwnerSQL
  , name :: !Text
  , age :: !Day
  , species :: !Species
  , sex :: !Sex
  , breed :: !(Maybe Text)
  , neutered :: !Bool
  , bloodType :: !(Maybe Text)
  , allergies :: !Text
  , createdAt :: !UTCTime
  }
  deriving (Show, Eq, Generic)

instance FromJSON PetSQL

instance SqlRow PetSQL

petsTable :: Table PetSQL
petsTable = table "pets" [#key :- autoPrimary]

data ConsultationSQL = ConsultationSQL
  { key :: ID ConsultationSQL
  , petKey :: ID PetSQL
  , weight :: !(Maybe Int)
  , time :: !UTCTime
  , motive :: !Text
  , diagnosis :: !(Maybe Text)
  , total :: !(Maybe Int)
  , paid :: !(Maybe Int)
  , remaining :: !(Maybe Int)
  , cancelled :: !Bool
  , createdAt :: !UTCTime
  }
  deriving (Show, Eq, Generic)

instance FromJSON ConsultationSQL

instance SqlRow ConsultationSQL

consultationsTable :: Table ConsultationSQL
consultationsTable = table "consultations" [#key :- autoPrimary]

data TreatmentSQL = TreatmentSQL
  { consultationKey :: ID ConsultationSQL
  , general :: !(Maybe Text)
  , actKey :: Maybe (ID ActSQL)
  , quantity :: Int
  , createdAt :: !UTCTime
  }
  deriving (Show, Eq, Generic)

instance FromJSON TreatmentSQL

instance SqlRow TreatmentSQL

treatmentsTable :: Table TreatmentSQL
treatmentsTable =
  table
    "treatments"
    [ #consultationKey :+ #actKey :- unique
    , #consultationKey :+ #general :- unique
    ]

data TokenSQL = TokenSQL
  { key :: ID TokenSQL
  , accessToken :: !Text
  , refreshToken :: !Text
  , expiresIn :: !UTCTime
  }
  deriving (Show, Eq, Generic)

instance SqlRow TokenSQL

tokenTable :: Table TokenSQL
tokenTable = table "token" [#key :- primary]

createTables :: (MonadSelda m) => m ()
createTables = do
  tryCreateTable passwordTable
  tryCreateTable actsTable
  tryCreateTable ownersTable
  tryCreateTable petsTable
  tryCreateTable treatmentsTable
  tryCreateTable consultationsTable
  tryCreateTable pricesTable
  tryCreateTable workHoursTable
  tryCreateTable tokenTable

migrateUp :: forall m. (MonadSelda m) => m ()
migrateUp = createTables
