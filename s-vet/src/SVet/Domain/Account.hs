module SVet.Domain.Account (
  Account (..),
) where

import Data.Aeson
import Optics.TH
import Relude
import SVet.Domain.Authentication
import SVet.Domain.Entities
import SVet.Domain.Internal
import Servant.Auth.JWT

data Account
  = VetAccount !Email
  | OwnerAccount !(EntKey Owner)
  | OutsiderAccount
  deriving (Show, Eq, Generic)

makeFieldLabelsWith noPrefixFieldLabels ''Account

instance ToJSON Account where
  toJSON (VetAccount mail) = object ["email" .= mail]
  toJSON (OwnerAccount ownerKey) = object ["key" .= ownerKey]
  toJSON OutsiderAccount = "Outsider"

  toEncoding (VetAccount mail) = pairs ("email" .= mail)
  toEncoding (OwnerAccount ownerKey) = pairs ("key" .= ownerKey)
  toEncoding OutsiderAccount = toEncoding @Text "Outsider"

instance FromJSON Account where
  parseJSON v = parseObj v <|> parseOutsider v
   where
    parseObj = withObject "Account" \o ->
      (VetAccount <$> o .: "email") <|> (OwnerAccount <$> o .: "key")
    parseOutsider = withText "Account" \case
      "Outsider" -> pure OutsiderAccount
      other -> fail $ "unknown account " <> toString other

instance ToJWT Account

instance FromJWT Account
