module SVet.API.Settings (SettingsAPI, settingsHandler) where

import Polysemy
import Polysemy.Error
import Relude
import SVet.API.Internal
import SVet.Authentication
import SVet.Domain
import SVet.Repository
import SVet.Tracing
import Servant

type SettingsServer api r =
  Members [SettingsRepository, Embed IO, Error AuthenticationError] r =>
  ServerT api (Sem r)

type GetSub = "settings" :> Get '[JSON] Settings

type GetAPI = SVetAuth :> GetSub

getHandler :: SettingsServer (Traced :> GetAPI) r
getHandler = serverWithVet @GetSub getSettings

type UpdateSub = "settings" :> ReqBody '[JSON] Settings :> Put '[JSON] NoContent

type UpdateAPI = SVetAuth :> UpdateSub

updateHandler :: SettingsServer (Traced :> UpdateAPI) r
updateHandler = serverWithVet @UpdateSub \v -> (NoContent <$) . updateSettings v

type SettingsAPI = GetAPI :<|> UpdateAPI

settingsHandler :: SettingsServer (TracedAPI SettingsAPI) r
settingsHandler = getHandler :<|> updateHandler
