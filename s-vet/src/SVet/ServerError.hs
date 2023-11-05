module SVet.ServerError (ToServerError (..)) where

import Relude
import Servant

class ToServerError e where
  toServerError :: e -> ServerError

instance ToServerError Text where
  toServerError = toServerError . toString

instance ToServerError String where
  toServerError s = err500{errReasonPhrase = s}

instance ToServerError () where
  toServerError _ = err500
