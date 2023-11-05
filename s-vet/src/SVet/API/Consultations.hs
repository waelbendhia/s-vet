{-# LANGUAGE AllowAmbiguousTypes #-}

module SVet.API.Consultations (ConsultationsAPI, consultationsHandler) where

import Data.Time
import Polysemy
import Polysemy.Error
import Relude
import SVet.API.Internal
import SVet.Authentication
import SVet.Domain
import SVet.Repository
import SVet.Tracing
import Servant

type SearchConsultationSub =
  "consultations"
    :> QueryParam "page" Int
    :> QueryParam "itemsPerPage" Int
    :> QueryParam "before" UTCTime
    :> QueryParam "after" UTCTime
    :> QueryParam "search" Text
    :> QueryParam "status" ConsultationStatus
    :> Get '[JSON] (SearchResult ConsultationWithPet)

type SearchConsultationAPI = SVetAuth :> SearchConsultationSub

searchConsultationHandler ::
  (Members [ConsultationRepository, Error AuthenticationError, Embed IO] r) =>
  ServerT (Traced :> SearchConsultationAPI) (Sem r)
searchConsultationHandler auth p ipp b a s st = do
  vetOrOwner <- mustBeAuthenticated auth
  searchConsultations vetOrOwner $
    ConsultationSearchRequest (max 0 <$> p ?: 0) (max 1 . min 50 <$> ipp ?: 10) b a s st

type InsertConsultationSub =
  "pets"
    :> Capture "key" (EntKey Pet)
    :> "consultations"
    :> ReqBody '[JSON] Consultation
    :> Post '[JSON] (Entity Consultation)

type InsertConsultationAPI = SVetAuth :> InsertConsultationSub

insertConsultationHandler ::
  (Members [ConsultationRepository, Error AuthenticationError, Embed IO] r) =>
  ServerT (Traced :> InsertConsultationAPI) (Sem r)
insertConsultationHandler auth k c = do
  v <- mustBeVet auth
  insertConsultation v k $ consultationEmptyFieldsToNothing c

type UpdateConsultationSub =
  "consultations"
    :> Capture "key" (EntKey Consultation)
    :> ReqBody '[JSON] Consultation
    :> Put '[JSON] (Entity Consultation)

type UpdateConsultationAPI = SVetAuth :> UpdateConsultationSub

updateConsultationHandler ::
  (Members [ConsultationRepository, Error AuthenticationError, Embed IO] r) =>
  ServerT (Traced :> UpdateConsultationAPI) (Sem r)
updateConsultationHandler auth cKey c = do
  v <- mustBeVet auth
  updateConsultation v $ WithField cKey $ consultationEmptyFieldsToNothing c

type DeleteConsultationSub =
  "consultations" :> Capture "key" (EntKey Consultation) :> Delete '[JSON] ()

type DeleteConsultationAPI = SVetAuth :> DeleteConsultationSub

deleteConsultationHandler ::
  (Members [ConsultationRepository, Error AuthenticationError, Embed IO] r) =>
  ServerT (Traced :> DeleteConsultationAPI) (Sem r)
deleteConsultationHandler auth k = do
  v <- mustBeVet auth
  deleteConsultation v k

type GetConsultationSub =
  "consultations"
    :> Capture "key" (EntKey Consultation)
    :> Get '[JSON] ConsultationWithPrevious

type GetConsultationAPI = SVetAuth :> GetConsultationSub

getConsultationHandler ::
  (Members [ConsultationRepository, Error AuthenticationError, Embed IO] r) =>
  ServerT (Traced :> GetConsultationAPI) (Sem r)
getConsultationHandler auth k = do
  acc <- mustBeAuthenticated auth
  getConsultationWithPrevious acc k

type ConsultationsAPI =
  GetConsultationAPI
    :<|> DeleteConsultationAPI
    :<|> UpdateConsultationAPI
    :<|> InsertConsultationAPI
    :<|> SearchConsultationAPI

consultationsHandler ::
  (Members [ConsultationRepository, Error AuthenticationError, Embed IO] r) =>
  ServerT (TracedAPI ConsultationsAPI) (Sem r)
consultationsHandler =
  getConsultationHandler
    :<|> deleteConsultationHandler
    :<|> updateConsultationHandler
    :<|> insertConsultationHandler
    :<|> searchConsultationHandler
