{-# OPTIONS_GHC -fno-warn-orphans #-}

module SVet.Repository.Tables (
  ActSQL (..),
  actsTable,
  TreatmentSQL (..),
  treatmentsTable,
  OwnerSQL (..),
  ownersTable,
  OwnerPasswordSQL (..),
  ownerPasswordsTable,
  ConsultationSQL (..),
  consultationsTable,
  PetSQL (..),
  petsTable,
  PriceSQL (..),
  pricesTable,
  WorkHoursSQL (..),
  workHoursTable,
  UserSQL (..),
  usersTable,
  TokenSQL (..),
  tokenTable,
  migrateUp,
  ilike,
  countAndLimit,
) where

import Control.Monad.Catch hiding (bracket)
import Data.Aeson
import Data.Text as T
import Data.Time
import Database.Selda as S hiding (toString, toText)
import Database.Selda.Migrations
import Database.Selda.Unsafe
import Database.Selda.Validation
import Optics
import Relude hiding (MonadReader, Reader, ask, local, runReader)
import SVet.Domain
import qualified SVet.Repository.Tables.V1 as V1

data UserSQL = UserSQL
  { password :: ByteString
  , createdAt :: UTCTime
  , email :: Email
  }
  deriving (Show, Eq, Generic)

instance SqlRow UserSQL

makeFieldLabelsWith noPrefixFieldLabels ''UserSQL

usersTable :: Table UserSQL
usersTable = table "users" [#email :- primary]

data WorkHoursSQL = WorkHoursSQL
  { start :: TimeOfDay
  , end :: TimeOfDay
  , vet :: Email
  , dayOfTheWeek :: WeekDay
  }
  deriving (Show, Eq, Generic)

instance FromJSON WorkHoursSQL

instance SqlRow WorkHoursSQL

makeFieldLabelsWith noPrefixFieldLabels ''WorkHoursSQL

workHoursTable :: Table WorkHoursSQL
workHoursTable = table "workhours" [(#vet :+ #dayOfTheWeek) :- primary]

data PriceSQL = PricesSQL
  { price :: Int
  , vet :: Email
  , priceType :: PriceType
  }
  deriving (Show, Eq, Generic)

instance FromJSON PriceSQL

instance SqlRow PriceSQL

makeFieldLabelsWith noPrefixFieldLabels ''PriceSQL

pricesTable :: Table PriceSQL
pricesTable =
  table "prices" [(#vet :+ #priceType) :- primary, #vet :- foreignKey usersTable #email]

data ActSQL = ActSQL
  { key :: ID ActSQL
  , price :: Int
  , createdAt :: UTCTime
  , vet :: Email
  , name :: Text
  }
  deriving (Show, Eq, Generic)

instance FromJSON ActSQL

type instance DomainEntity ActSQL = Act

makeFieldLabelsWith noPrefixFieldLabels ''ActSQL

instance SqlRow ActSQL

actsTable :: Table ActSQL
actsTable =
  table "acts" [#key :- autoPrimary, #vet :- foreignKey usersTable #email]

data OwnerSQL = OwnerSQL
  { key :: ID OwnerSQL
  , createdAt :: UTCTime
  , name :: Text
  , phonenumber :: Maybe Text
  , address :: Maybe Text
  , email :: Maybe Email
  , vet :: Email
  }
  deriving (Show, Eq, Generic)

instance FromJSON OwnerSQL

type instance DomainEntity OwnerSQL = Owner

instance AsEntity OwnerSQL where
  asEntity r =
    WithField
      (fromSeldaID $ r ^. #key)
      Owner
        { name = r ^. #name
        , email = r ^. #email
        , phonenumber = r ^. #phonenumber
        , address = r ^. #address
        }

makeFieldLabelsWith noPrefixFieldLabels ''OwnerSQL

instance SqlRow OwnerSQL

ownersTable :: Table OwnerSQL
ownersTable =
  table
    "owners"
    [ #key :- autoPrimary
    , #email :- unique
    , #phonenumber :- unique
    , #vet :- foreignKey usersTable #email
    ]

data OwnerPasswordSQL = OwnerPasswordSQL
  { ownerKey :: ID OwnerSQL
  , password :: ByteString
  , createdAt :: UTCTime
  }
  deriving (Generic)

instance SqlRow OwnerPasswordSQL

ownerPasswordsTable :: Table OwnerPasswordSQL
ownerPasswordsTable =
  table
    "owner_passwords"
    [ #ownerKey :- primary
    , #ownerKey :- foreignKey ownersTable #key
    , #password :- unique
    ]

data PetSQL = PetSQL
  { key :: ID PetSQL
  , ownerKey :: ID OwnerSQL
  , name :: Text
  , age :: Day
  , species :: Species
  , sex :: Sex
  , breed :: Maybe Text
  , neutered :: Bool
  , bloodType :: Maybe Text
  , allergies :: Text
  , createdAt :: UTCTime
  }
  deriving (Show, Eq, Generic)

instance FromJSON PetSQL

type instance DomainEntity PetSQL = Pet

instance AsEntity PetSQL where
  asEntity p =
    WithField
      (fromSeldaID $ p ^. #key)
      Pet
        { name = p ^. #name
        , age = p ^. #age
        , species = p ^. #species
        , sex = p ^. #sex
        , breed = p ^. #breed
        , neutered = p ^. #neutered
        , bloodType = p ^. #bloodType
        , allergies =
            decodeStrict (encodeUtf8 $ p ^. #allergies) ?:
              error "invalid allergies"
        }

makeFieldLabelsWith noPrefixFieldLabels ''PetSQL

instance SqlRow PetSQL

petsTable :: Table PetSQL
petsTable = table "pets" [#key :- autoPrimary]

data ConsultationSQL = ConsultationSQL
  { key :: ID ConsultationSQL
  , petKey :: ID PetSQL
  , weight :: Maybe Int
  , time :: UTCTime
  , motive :: Text
  , diagnosis :: Maybe Text
  , total :: Maybe Int
  , paid :: Maybe Int
  , remaining :: Maybe Int
  , cancelled :: Bool
  , createdAt :: UTCTime
  }
  deriving (Show, Eq, Generic)

type instance DomainEntity ConsultationSQL = Consultation

instance AsEntity ConsultationSQL where
  asEntity c =
    WithField
      (fromSeldaID $ c ^. #key)
      Consultation
        { weight = c ^. #weight
        , motive = c ^. #motive
        , diagnosis = c ^. #diagnosis
        , treatment = []
        , time = c ^. #time
        , amount =
            Amount
              <$> c ^. #total
              <*> c ^. #paid
              <*> c ^. #remaining
        }

instance FromJSON ConsultationSQL

instance SqlRow ConsultationSQL

makeFieldLabelsWith noPrefixFieldLabels ''ConsultationSQL

consultationsTable :: Table ConsultationSQL
consultationsTable = table "consultations" [#key :- autoPrimary]

data TreatmentSQL = TreatmentSQL
  { consultationKey :: ID ConsultationSQL
  , general :: Maybe Text
  , actKey :: Maybe (ID ActSQL)
  , quantity :: Int
  , createdAt :: UTCTime
  }
  deriving (Show, Eq, Generic)

instance FromJSON TreatmentSQL

instance SqlRow TreatmentSQL

makeFieldLabelsWith noPrefixFieldLabels ''TreatmentSQL

treatmentsTable :: Table TreatmentSQL
treatmentsTable =
  table
    "treatments"
    [ #consultationKey :+ #actKey :- unique
    , #consultationKey :+ #general :- unique
    ]

data TokenSQL = TokenSQL
  { key :: ID TokenSQL
  , accessToken :: Text
  , refreshToken :: Text
  , expiresIn :: UTCTime
  }
  deriving (Show, Eq, Generic)

makeFieldLabelsWith noPrefixFieldLabels ''TokenSQL

instance SqlRow TokenSQL

tokenTable :: Table TokenSQL
tokenTable = table "token" [#key :- primary]

addEmailToPasswordTable :: UTCTime -> Migration backend
addEmailToPasswordTable now =
  Migration V1.passwordTable usersTable $ \r ->
    pure $
      new
        [ #email := literal defaultEmail
        , #password := r ! #password
        , #createdAt := literal now
        ]

addEmailToPricesTable :: Migration backend
addEmailToPricesTable =
  Migration V1.pricesTable pricesTable $ \r ->
    pure $ new [#vet := literal defaultEmail, #priceType := r ! #priceType, #price := r ! #price]

addEmailToWorkHoursTable :: Migration backend
addEmailToWorkHoursTable =
  Migration V1.workHoursTable workHoursTable $ \r ->
    pure $
      new
        [ #vet := literal defaultEmail
        , #dayOfTheWeek := r ! #dayOfTheWeek
        , #start := r ! #start
        , #end := r ! #end
        ]

addEmailToActsTable :: Migration backend
addEmailToActsTable =
  Migration V1.actsTable actsTable $ \r ->
    pure $
      new
        [ #key := cast (r ! #key)
        , #vet := literal defaultEmail
        , #price := r ! #price
        , #name := r ! #name
        , #createdAt := r ! #createdAt
        ]

addEmailToOwnersTable :: Migration backend
addEmailToOwnersTable =
  Migration V1.ownersTable ownersTable $ \r ->
    pure $
      new
        [ #key := cast (r ! #key)
        , #vet := literal defaultEmail
        , #name := r ! #name
        , #email := r ! #email
        , #phonenumber := r ! #phonenumber
        , #address := r ! #address
        , #createdAt := r ! #createdAt
        ]

migrateUp :: forall m. (MonadMask m, MonadSelda m) => m ()
migrateUp = do
  V1.migrateUp
  now <- liftIO @m getCurrentTime
  autoMigrate
    False
    [
      [ addEmailToPasswordTable @(Backend m) now
      , addEmailToPricesTable @(Backend m)
      , addEmailToWorkHoursTable @(Backend m)
      , addEmailToActsTable @(Backend m)
      , addEmailToOwnersTable @(Backend m)
      ]
    ]
  createTable ownerPasswordsTable
 where
  _printTableDiff t =
    putStrLn
      . (("diffing table " <> show (tableName t) <> " ") <>)
      . toString
      . showTableDiff
      =<< diffTable t
  _printDiff (Migration src _ _) = _printTableDiff src
  _printDiffTgt (Migration _ t _) = _printTableDiff t

ilike :: forall (t :: Type) a. Col t a -> Text -> Col t Bool
ilike col t = fun "UPPER" col `like` literal ("%" <> T.toUpper t <> "%")

countAndLimit ::
  ( Res (AggrCols (Aggr s Int)) ~ Int
  , MonadSelda f
  , S.Result (OuterCols a1)
  , S.Result (AggrCols (Aggr s Int))
  , Columns (AggrCols (Aggr s Int))
  , Aggregates (Aggr s Int)
  , SqlType a2
  ) =>
  Int ->
  Int ->
  (a1 -> Col s a2) ->
  Query (Inner (Backend f)) a1 ->
  f (SearchResult (Res (OuterCols a1)))
countAndLimit p ipp s q =
  SearchResult
    <$> query (limit (p * ipp) ipp q)
    <*> fmap ((?: 0) . listToMaybe) (query $ aggregate $ S.count . s <$> q)
