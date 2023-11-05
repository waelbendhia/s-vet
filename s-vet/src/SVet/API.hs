module SVet.API (API, server, module SVet.API.Authentication) where

import Database.Selda
import Polysemy
import Polysemy.Error
import Polysemy.Input
import Relude
import SVet.API.Acts
import SVet.API.Authentication
import SVet.API.Consultations
import SVet.API.Owners
import SVet.API.Pets
import SVet.API.Settings
import SVet.API.Statistics
import SVet.API.Treatments
import SVet.Authentication
import qualified SVet.Logging as SVet
import SVet.Repository
import SVet.Tracing
import Servant

type API =
  "api"
    :> TracedAPI
        ( AuthenticationAPI
            :<|> ConsultationsAPI
            :<|> PetsAPI
            :<|> OwnersAPI
            :<|> ActsAPI
            :<|> SettingsAPI
            :<|> TreatmentsAPI
            :<|> StatisticsAPI
        )

server ::
  Members
    [ Session
    , Error ActValidationError
    , Input UTCTime
    , Crypt
    , Error AuthenticationError
    , ConsultationRepository
    , OwnerRepository
    , PetRepository
    , SettingsRepository
    , TreatmentRepository
    , Statistics
    , ActRepository
    , Embed IO
    , SVet.KatipContext
    ]
    r =>
  ServerT API (Sem r)
server =
  authenticationHandler
    :<|> consultationsHandler
    :<|> petsHandler
    :<|> ownersHandler
    :<|> actsHandler
    :<|> settingsHandler
    :<|> treatmentsHandler
    :<|> statisticsHandler
