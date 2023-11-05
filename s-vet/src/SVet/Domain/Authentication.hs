module SVet.Domain.Authentication (
  ChangePasswordRequest (..),
  Token (..),
  GoogleClientCredentials (..),
  Email,
  defaultEmail,
  Password,
  defaultPassword,
) where

import Data.Aeson
import Data.Aeson.TH
import Database.Selda
import Database.Selda.SqlType
import Optics
import Relude
import SVet.TH (lookupCompileEnvExp)

defaultEmail :: Email
defaultEmail = Email $(lookupCompileEnvExp "S_VET_DEFAULT_EMAIL" "cvazn@gmail.com")

newtype Email = Email Text
  deriving (Show, Eq)

instance ToText Email where toText (Email t) = t

instance ToJSON Email where toJSON (Email t) = toJSON t

instance FromJSON Email where parseJSON v = Email <$> parseJSON v

instance SqlType Email where
  fromSql = Email . fromSql
  defaultValue = mkLit (Email "")
  mkLit (Email t) = LCustom TText (LText t)

defaultPassword :: Password
defaultPassword = Password $(lookupCompileEnvExp "S_VET_DEFAULT_PASSWORD" "changeme")

newtype Password = Password Text
  deriving (ToText, FromJSON) via Text

data ChangePasswordRequest = ChangePasswordRequest
  { oldPassword :: !Password
  , newPassword :: !Password
  }
  deriving (Generic)

instance FromJSON ChangePasswordRequest

makeFieldLabelsWith noPrefixFieldLabels ''ChangePasswordRequest

data Token = Token
  { accessToken :: !Text
  , expiresIn :: !Int
  , tokenType :: !Text
  , scope :: !Text
  , refreshToken :: !(Maybe Text)
  }
  deriving (Show, Eq)

makeFieldLabelsWith noPrefixFieldLabels ''Token

deriveJSON
  defaultOptions
    { fieldLabelModifier = \case
        "accessToken" -> "access_token"
        "expiresIn" -> "expires_in"
        "tokenType" -> "token_type"
        "scope" -> "scope"
        "refreshToken" -> "refresh_token"
        x -> x
    }
  ''Token

data GoogleClientCredentials = GoogleClientCredentials {clientID :: Text, clientSecret :: Text}
  deriving (Show, Eq, Generic)

makeFieldLabelsWith noPrefixFieldLabels ''GoogleClientCredentials
