module SVet.Authentication (AuthenticationError (..)) where

import Relude
import SVet.Domain

data AuthenticationError = UnauthorizedAccess | UserNotFound !Email | IncorrectPassword !Email
  deriving (Show)
