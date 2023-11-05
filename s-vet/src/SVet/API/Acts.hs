module SVet.API.Acts (ActsAPI, actsHandler, ActValidationError (..)) where

import Data.Text as T (null)
import Optics
import Polysemy
import Polysemy.Error
import Relude
import SVet.API.Internal
import SVet.Authentication
import SVet.Domain
import SVet.Repository
import SVet.ServerError
import SVet.Tracing
import Servant

data ActValidationError = InvalidName | InvalidPrice deriving (Show, Eq)

instance ToServerError ActValidationError where
  toServerError InvalidPrice =
    err400
      { errReasonPhrase = "invalid act price"
      , errBody = "invalid act price"
      }
  toServerError InvalidName =
    err400
      { errReasonPhrase = "invalid act name"
      , errBody = "invalid act name"
      }

withValidation :: Members '[Error ActValidationError] r => (Act -> Sem r a) -> Act -> Sem r a
withValidation f a = do
  when (T.null (a ^. #name)) (throw InvalidName)
  when (a ^. #price <= 0) (throw InvalidPrice)
  f a

type ActServer api r =
  ( Members
      '[ ActRepository
       , Error ActValidationError
       , Embed IO
       , Error AuthenticationError
       ]
      r
  ) =>
  ServerT api (Sem r)

type InsertSub = "acts" :> ReqBody '[JSON] Act :> Post '[JSON] (Entity Act)

type InsertAPI = SVetAuth :> InsertSub

insertHandler :: ActServer (Traced :> InsertAPI) r
insertHandler = serverWithVet @InsertSub (withValidation . insertAct)

type UpdateSub =
  "acts"
    :> Capture "key" (EntKey Act)
    :> ReqBody '[JSON] Act
    :> Put '[JSON] (Entity Act)

type UpdateAPI = SVetAuth :> UpdateSub

updateHandler :: ActServer (Traced :> UpdateAPI) r
updateHandler = serverWithVet @UpdateSub \v k -> withValidation (updateAct v . WithField k)

type GetSub = "acts" :> Capture "key" (EntKey Act) :> Get '[JSON] (Entity Act)

type GetAPI = SVetAuth :> GetSub

getHandler :: ActServer (Traced :> GetAPI) r
getHandler = serverWithVet @GetSub getAct

type DeleteSub = "acts" :> Capture "key" (EntKey Act) :> Delete '[JSON] NoContent

type DeleteAPI = SVetAuth :> DeleteSub

deleteHandler :: ActServer (Traced :> DeleteAPI) r
deleteHandler = serverWithVet @DeleteSub \v k -> NoContent <$ deleteAct v k

type SearchSub = "acts" :> QueryParam "search" Text :> Get '[JSON] [Entity Act]

type SearchAPI = SVetAuth :> SearchSub

searchHandler :: ActServer (Traced :> SearchAPI) r
searchHandler = serverWithVet @SearchSub searchActs

type ActsAPI = InsertAPI :<|> UpdateAPI :<|> GetAPI :<|> DeleteAPI :<|> SearchAPI

actsHandler :: ActServer (TracedAPI ActsAPI) r
actsHandler = insertHandler :<|> updateHandler :<|> getHandler :<|> deleteHandler :<|> searchHandler
