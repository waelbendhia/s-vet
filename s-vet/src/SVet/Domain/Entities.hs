module SVet.Domain.Entities (
  SearchResult (..),
  AdminLogin (..),
  Amount (..),
  Consultation (..),
  Owner (..),
  Pet (..),
  Species (..),
  Sex (..),
  Act (..),
  GeneralOrSpecific (..),
  AppliedTreatment (..),
  AppliedTreatmentKey,
  PetWithOwner,
  PetWithConsultations,
  ConsultationWithPet,
  ConsultationWithPrevious,
  OwnerWithPetsAndBalance,
  ConsultationStatus (..),
  ConsultationSearchRequest (..),
  PetSearchRequest (..),
  OwnerSearchRequest (..),
  _General,
  _Specific,
  consultationEmptyFieldsToNothing,
  petEmptyFieldsToNothing,
  ownerEmtpyFieldsToNothing,
) where

import Data.Aeson
import Data.Aeson.TH
import qualified Data.Text as T
import Data.Time
import Data.Time.Clock.POSIX
import Database.Selda hiding (toText)
import Optics
import Relude
import SVet.Domain.Authentication
import SVet.Domain.Internal
import Servant

data Amount = Amount {total :: !Int, paid :: !Int, remaining :: !Int}
  deriving (Show, Read, Eq, Generic)

makeFieldLabelsWith noPrefixFieldLabels ''Amount

deriveJSON defaultOptions ''Amount

data Act = Act {price :: !Int, name :: !Text}
  deriving (Show, Eq, Generic)

makeFieldLabelsWith noPrefixFieldLabels ''Act

deriveJSON defaultOptions ''Act

data GeneralOrSpecific general specific = General general | Specific specific
  deriving (Show, Eq)

instance
  (FromHttpApiData general, FromHttpApiData specific) =>
  FromHttpApiData (GeneralOrSpecific general specific)
  where
  parseUrlPiece t = (Specific <$> parseUrlPiece t) <> (General <$> parseUrlPiece t)

makePrisms ''GeneralOrSpecific

deriveJSON defaultOptions ''GeneralOrSpecific

type AppliedTreatmentKey = GeneralOrSpecific Text (EntKey Act)

data AppliedTreatment = AppliedTreatment
  { quantity :: !Int
  , treatment :: !(GeneralOrSpecific Text (Entity Act))
  }
  deriving (Show, Eq)

makeFieldLabelsWith noPrefixFieldLabels ''AppliedTreatment

deriveJSON defaultOptions ''AppliedTreatment

data Consultation = Consultation
  { weight :: !(Maybe Int)
  , motive :: !Text
  , diagnosis :: !(Maybe Text)
  , treatment :: ![AppliedTreatment]
  , time :: !UTCTime
  , amount :: !(Maybe Amount)
  }
  deriving (Show, Eq, Generic)

makeFieldLabelsWith noPrefixFieldLabels ''Consultation

deriveJSON defaultOptions ''Consultation

consultationEmptyFieldsToNothing :: Consultation -> Consultation
consultationEmptyFieldsToNothing =
  over #diagnosis (mfilter (not . T.null)) . over #weight (mfilter (/= 0))

data Species = Cat | Dog
  deriving (Show, Read, Bounded, Enum, Eq, Generic)

deriveJSON defaultOptions ''Species

instance SqlType Species

data Sex = Male | Female
  deriving (Show, Read, Bounded, Enum, Eq, Generic)

deriveJSON defaultOptions ''Sex

instance SqlType Sex

data Pet = Pet
  { name :: !Text
  , age :: !Day
  , species :: !Species
  , sex :: !Sex
  , breed :: !(Maybe Text)
  , neutered :: !Bool
  , bloodType :: !(Maybe Text)
  , allergies :: ![Text]
  }
  deriving (Show, Eq, Generic)

makeFieldLabelsWith noPrefixFieldLabels ''Pet

deriveJSON defaultOptions ''Pet

petEmptyFieldsToNothing :: Pet -> Pet
petEmptyFieldsToNothing =
  over #breed (mfilter (not . T.null))
    . over #bloodType (mfilter (not . T.null))

data Owner = Owner
  { name :: !Text
  , email :: !(Maybe Email)
  , phonenumber :: !(Maybe Text)
  , address :: !(Maybe Text)
  }
  deriving (Show, Eq, Generic)

makeFieldLabelsWith noPrefixFieldLabels ''Owner

deriveJSON defaultOptions ''Owner

ownerEmtpyFieldsToNothing :: Owner -> Owner
ownerEmtpyFieldsToNothing =
  over #email (mfilter (not . T.null . toText))
    . over #phonenumber (mfilter (not . T.null))
    . over #address (mfilter (not . T.null))

type PetWithOwner = WithField "owner" (Entity Owner) (Entity Pet)

type PetWithConsultations = WithField "consultations" [Entity Consultation] PetWithOwner

type ConsultationWithPet = WithField "pet" PetWithOwner (Entity Consultation)

type ConsultationWithPrevious =
  WithField "previous" [Entity Consultation] ConsultationWithPet

type OwnerWithPetsAndBalance =
  WithField
    "paid"
    Int
    ( WithField
        "unpaid"
        Int
        (WithField "pets" [Entity Pet] (Entity Owner))
    )

newtype AdminLogin = AdminLogin Text

data SearchResult d = SearchResult {rows :: ![d], total :: !Int}
  deriving (Show, Eq)

makeFieldLabelsWith noPrefixFieldLabels ''SearchResult

deriveJSON defaultOptions ''SearchResult

data ConsultationStatus = Pending | Completed
  deriving (Read, Show, Eq, Enum)

instance FromHttpApiData ConsultationStatus where
  parseUrlPiece t = case T.toLower t of
    "pending" -> pure Pending
    "completed" -> pure Completed
    other -> Left $ "unknown status " <> other

instance Hashable ConsultationStatus where
  hashWithSalt salt = hashWithSalt salt . fromEnum

data ConsultationSearchRequest = ConsultationSearchRequest
  { page :: !Int
  , itemsPerPage :: !Int
  , before :: !(Maybe UTCTime)
  , after :: !(Maybe UTCTime)
  , term :: !(Maybe Text)
  , status :: !(Maybe ConsultationStatus)
  }
  deriving (Show, Read, Eq)

makeFieldLabelsWith noPrefixFieldLabels ''ConsultationSearchRequest

instance Hashable ConsultationSearchRequest where
  hashWithSalt salt (ConsultationSearchRequest p ipp b a t s) =
    hashWithSalt
      salt
      ( p
      , ipp
      , nominalDiffTimeToSeconds . utcTimeToPOSIXSeconds <$> b
      , nominalDiffTimeToSeconds . utcTimeToPOSIXSeconds <$> a
      , t
      , s
      )

data PetSearchRequest = PetSearchRequest
  { page :: !Int
  , itemsPerPage :: !Int
  , search :: !(Maybe Text)
  }
  deriving (Show, Read, Eq)

makeFieldLabelsWith noPrefixFieldLabels ''PetSearchRequest

instance Hashable PetSearchRequest where
  hashWithSalt salt (PetSearchRequest p ipp s) =
    hashWithSalt salt (p, ipp, s)

data OwnerSearchRequest = OwnerSearchRequest
  { page :: !Int
  , itemsPerPage :: !Int
  , search :: !(Maybe Text)
  }
  deriving (Show, Eq)

makeFieldLabelsWith noPrefixFieldLabels ''OwnerSearchRequest

instance Hashable OwnerSearchRequest where
  hashWithSalt salt (OwnerSearchRequest p ipp s) =
    hashWithSalt salt (p, ipp, s)
