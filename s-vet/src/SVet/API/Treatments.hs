module SVet.API.Treatments (TreatmentsAPI, treatmentsHandler) where

import Polysemy
import Polysemy.Error
import Relude
import SVet.API.Internal
import SVet.Authentication
import SVet.Domain
import SVet.Repository
import SVet.Tracing
import Servant

type TreatmentsServer api r =
  Members
    [ TreatmentRepository
    , Error AuthenticationError
    , Embed IO
    , Error AuthenticationError
    ]
    r =>
  ServerT api (Sem r)

type InsertSub =
  "consultations"
    :> Capture "key" (EntKey Consultation)
    :> ReqBody '[JSON] AppliedTreatment
    :> Post '[JSON] AppliedTreatment

type InsertAPI = SVetAuth :> InsertSub

insertHandler :: TreatmentsServer (Traced :> InsertAPI) r
insertHandler = serverWithVet @InsertSub insertTreatment

type DeleteSub =
  "consultations"
    :> Capture "key" (EntKey Consultation)
    :> "treatments"
    :> Capture "treatment" AppliedTreatmentKey
    :> Delete '[JSON] NoContent

type DeleteAPI = SVetAuth :> DeleteSub

deleteHandler :: TreatmentsServer (Traced :> DeleteAPI) r
deleteHandler = serverWithVet @DeleteSub \v k t -> NoContent <$ deleteTreatment v k t

type GetsSub =
  "consultations"
    :> Capture "key" (EntKey Consultation)
    :> "treatments"
    :> Get '[JSON] [AppliedTreatment]

type GetsAPI = SVetAuth :> GetsSub

getsHandler :: TreatmentsServer (Traced :> GetsAPI) r
getsHandler = serverWithVet @GetsSub getTreatmentsByConsultation

type TreatmentsAPI = InsertAPI :<|> DeleteAPI :<|> GetsAPI

treatmentsHandler :: TreatmentsServer (TracedAPI TreatmentsAPI) r
treatmentsHandler = insertHandler :<|> deleteHandler :<|> getsHandler
