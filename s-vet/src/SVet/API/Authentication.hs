{-# LANGUAGE BlockArguments #-}

module SVet.API.Authentication (
  Session (..),
  LoginAPI,
  loginHandler,
  Login (..),
  AuthenticationAPI,
  authenticationHandler,
  runSession,
) where

import Data.Aeson
import Data.Text
import Katip
import Optics
import Polysemy
import Polysemy.Error
import Relude hiding (Reader, ask)
import SVet.API.Internal
import SVet.Authentication
import SVet.Domain
import qualified SVet.Logging as SVet
import SVet.Repository
import SVet.Tracing
import Servant
import Servant.Auth.Server

data Login = Login {email :: !Email, password :: !Password} deriving (Generic)

instance FromJSON Login

makeFieldLabelsWith noPrefixFieldLabels ''Login

type WithTwoCookies a =
  Headers '[Header "Set-Cookie" SetCookie, Header "Set-Cookie" SetCookie] a

data Session m a where
  StartSession :: Account -> Session m (WithTwoCookies Account)
  EndSession :: a -> Session m (WithTwoCookies a)

makeSem ''Session

runSession ::
  Members '[Embed IO, Error AuthenticationError] r =>
  CookieSettings ->
  JWTSettings ->
  Sem (Session : r) a ->
  Sem r a
runSession cookieSettings jwtSettings = interpret \case
  StartSession creds -> do
    t <- liftIO $ acceptLogin cookieSettings jwtSettings creds
    note UnauthorizedAccess (t ?? creds)
  EndSession resp -> pure $ clearSession cookieSettings resp

type LoginAPI =
  "login"
    :> ReqBody '[JSON] Login
    :> Post
        '[JSON]
        ( Headers
            '[Header "Set-Cookie" SetCookie, Header "Set-Cookie" SetCookie]
            Account
        )

loginHandler ::
  ( Members
      [ SettingsRepository
      , Crypt
      , Error AuthenticationError
      , Session
      , Embed IO
      , SVet.KatipContext
      ]
      r
  ) =>
  ServerT (Traced :> LoginAPI) (Sem r)
loginHandler l = katipAddContext (sl "email" (l ^. #email)) do
  $logTM DebugS "received login"
  maybePwd <- getPassword $ l ^. #email
  pwd <- note UnauthorizedAccess maybePwd
  $logTM DebugS "found matching password"
  unlessM
    (toText (l ^. #password) `compareToHash` pwd)
    (throw UnauthorizedAccess)
  $logTM DebugS "authentication successful"
  startSession (VetAccount $ l ^. #email)

data OwnerLogin = OwnerLogin
  { key :: !(EntKey Owner)
  , password :: !Password
  }
  deriving (Generic)

instance FromJSON OwnerLogin where
  parseJSON = withObject "OwnerLogin" $ \o -> do
    pwd <- o .: "password"
    case splitOn "-" pwd of
      [k, pwd'] -> OwnerLogin <$> parseJSON (String k) <*> parseJSON (String pwd')
      _ -> fail "invalid password"

type OwnerLoginAPI =
  "login"
    :> "owners"
    :> ReqBody '[JSON] OwnerLogin
    :> Post
        '[JSON]
        (Headers '[Header "Set-Cookie" SetCookie, Header "Set-Cookie" SetCookie] Account)

ownerLoginHandler ::
  ( Members [OwnerRepository, Crypt, Error AuthenticationError, Session, Embed IO] r
  , KatipContext (Sem r)
  ) =>
  ServerT (Traced :> OwnerLoginAPI) (Sem r)
ownerLoginHandler (OwnerLogin okey pwd) = katipAddContext (sl "owner-key" okey) $ do
  $logTM DebugS "received owner login"
  maybePwd <- getOwnerPassword okey
  opwd <- note UnauthorizedAccess maybePwd
  $logTM DebugS "found matching password"
  unlessM
    (toText pwd `compareToHash` opwd)
    (throw UnauthorizedAccess)
  $logTM DebugS "authentication successful"
  startSession (OwnerAccount okey)

type AccountSub = "account" :> Get '[JSON] Account

type AccountAPI = SVetAuth :> AccountSub

accountHandler :: ServerT (Traced :> AccountAPI) (Sem r)
accountHandler (Authenticated (VetAccount mail)) = pure $ VetAccount mail
accountHandler _other = pure OutsiderAccount

type LogoutAPI =
  "logout"
    :> Post
        '[JSON]
        (Headers '[Header "Set-Cookie" SetCookie, Header "Set-Cookie" SetCookie] NoContent)

logoutHandler :: Members [Session, Embed IO] r => ServerT (Traced :> LogoutAPI) (Sem r)
logoutHandler = endSession NoContent

type ChangePasswordSub =
  "change-password"
    :> ReqBody '[JSON] ChangePasswordRequest
    :> Post '[JSON] NoContent

type ChangePasswordAPI = SVetAuth :> ChangePasswordSub

changePasswordHandler ::
  (Members [SettingsRepository, Crypt, Error AuthenticationError, Embed IO] r) =>
  ServerT (Traced :> ChangePasswordAPI) (Sem r)
changePasswordHandler auth (ChangePasswordRequest old new) = do
  v <- mustBeVet auth
  maybePwd <- getPassword v
  pwd <- note (UserNotFound v) maybePwd
  unlessM
    (compareToHash (toText old) pwd)
    (throw (IncorrectPassword v))
  updatePassword v new
  pure NoContent

type AuthenticationAPI =
  LoginAPI :<|> OwnerLoginAPI :<|> LogoutAPI :<|> AccountAPI :<|> ChangePasswordAPI

authenticationHandler ::
  ( Members
      [ SettingsRepository
      , Crypt
      , Session
      , Error AuthenticationError
      , OwnerRepository
      , Embed IO
      , SVet.KatipContext
      ]
      r
  ) =>
  ServerT (TracedAPI AuthenticationAPI) (Sem r)
authenticationHandler =
  loginHandler
    :<|> ownerLoginHandler
    :<|> logoutHandler
    :<|> accountHandler
    :<|> changePasswordHandler
