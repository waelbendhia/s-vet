module SVet.API.Pets (PetsAPI, petsHandler) where

import Polysemy
import Polysemy.Error
import Relude
import SVet.API.Internal
import SVet.Authentication
import SVet.Domain
import SVet.Repository
import SVet.Tracing
import Servant

type SearchSub =
  "pets"
    :> QueryParam "page" Int
    :> QueryParam "itemsPerPage" Int
    :> QueryParam "search" Text
    :> Get '[JSON] (SearchResult PetWithOwner)

type SearchAPI = SVetAuth :> SearchSub

searchHandler ::
  Members [Error AuthenticationError, PetRepository, Embed IO] r =>
  ServerT (Traced :> SearchAPI) (Sem r)
searchHandler = serverWithAccount @SearchSub \a p ipp ->
  searchPets a . PetSearchRequest (max 0 $ p ?: 0) (max 1 $ min 50 $ ipp ?: 10)

type UpdateSub =
  Capture "key" (EntKey Pet) :> ReqBody '[JSON] Pet :> Put '[JSON] (Entity Pet)

type UpdateAPI =
  SVetAuth
    :> "pets"
    :> Capture "key" (EntKey Pet)
    :> ReqBody '[JSON] Pet
    :> Put '[JSON] (Entity Pet)

updateHandler ::
  Members [Error AuthenticationError, PetRepository, Embed IO] r =>
  ServerT (Traced :> UpdateAPI) (Sem r)
updateHandler = serverWithVet @UpdateSub \v k -> updatePet v k . petEmptyFieldsToNothing

type GetSub = "pets" :> Capture "key" (EntKey Pet) :> Get '[JSON] PetWithConsultations

type GetAPI = SVetAuth :> GetSub

getHandler ::
  Members [Error AuthenticationError, PetRepository, Embed IO] r =>
  ServerT (Traced :> GetAPI) (Sem r)
getHandler = serverWithAccount @GetSub getPet

type PetsAPI = UpdateAPI :<|> SearchAPI :<|> GetAPI

petsHandler ::
  Members [Error AuthenticationError, PetRepository, Embed IO] r =>
  ServerT (TracedAPI PetsAPI) (Sem r)
petsHandler = updateHandler :<|> searchHandler :<|> getHandler
