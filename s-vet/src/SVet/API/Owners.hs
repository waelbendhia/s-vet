module SVet.API.Owners (OwnersAPI, ownersHandler) where

import Polysemy
import Polysemy.Error
import Relude
import SVet.API.Internal
import SVet.Authentication
import SVet.Domain
import SVet.Repository
import SVet.Tracing
import Servant

type InsertSub = "owners" :> ReqBody '[JSON] Owner :> Post '[JSON] (Entity Owner)

type InsertAPI = SVetAuth :> InsertSub

insertHandler ::
  (Members [Embed IO, Error AuthenticationError, OwnerRepository] r) =>
  ServerT (Traced :> InsertAPI) (Sem r)
insertHandler = serverWithVet @InsertSub \v -> insertOwner v . ownerEmtpyFieldsToNothing

type InsertPetSub =
  "owners"
    :> Capture "key" (EntKey Owner)
    :> "pets"
    :> ReqBody '[JSON] Pet
    :> Post '[JSON] (Entity Pet)

type InsertPetAPI = SVetAuth :> InsertPetSub

insertPetHandler ::
  (Members [Embed IO, Error AuthenticationError, PetRepository] r) =>
  ServerT (Traced :> InsertPetAPI) (Sem r)
insertPetHandler = serverWithVet @InsertPetSub \v k -> insertPet v k . petEmptyFieldsToNothing

type UpdateSub =
  "owners"
    :> Capture "key" (EntKey Owner)
    :> ReqBody '[JSON] Owner
    :> Put '[JSON] (Entity Owner)

type UpdateAPI = SVetAuth :> UpdateSub

updateHandler ::
  (Members [Embed IO, Error AuthenticationError, OwnerRepository] r) =>
  ServerT (Traced :> UpdateAPI) (Sem r)
updateHandler = serverWithVet @UpdateSub \v k -> updateOwner v k . ownerEmtpyFieldsToNothing

type GetSub = "owners" :> Capture "key" (EntKey Owner) :> Get '[JSON] OwnerWithPetsAndBalance

type GetAPI = SVetAuth :> GetSub

getHandler ::
  (Members [Embed IO, Error AuthenticationError, OwnerRepository] r) =>
  ServerT (Traced :> GetAPI) (Sem r)
getHandler = serverWithVet @GetSub getOwner

type SearchSub =
  "owners"
    :> QueryParam "page" Int
    :> QueryParam "itemsPerPage" Int
    :> QueryParam "search" Text
    :> Get '[JSON] (SearchResult (Entity Owner))

type SearchAPI = SVetAuth :> SearchSub

searchHandler ::
  (Members [Embed IO, Error AuthenticationError, OwnerRepository] r) =>
  ServerT (Traced :> SearchAPI) (Sem r)
searchHandler = serverWithVet @SearchSub \v p ipp ->
  searchOwners v . OwnerSearchRequest (max 0 $ p ?: 0) (max 1 $ min 50 $ ipp ?: 0)

type CreatePasswordSub = "owners" :> Capture "key" (EntKey Owner) :> "password" :> Post '[JSON] Text

type CreatePasswordAPI = SVetAuth :> CreatePasswordSub

createPasswordHandler ::
  (Members [Embed IO, Error AuthenticationError, OwnerRepository] r) =>
  ServerT (Traced :> CreatePasswordAPI) (Sem r)
createPasswordHandler =
  serverWithVet @CreatePasswordSub \v k -> ((show k <> "-") <>) <$> createOwnerPassword v k

type OwnersAPI =
  InsertAPI :<|> InsertPetAPI :<|> UpdateAPI :<|> GetAPI :<|> SearchAPI :<|> CreatePasswordAPI

ownersHandler ::
  (Members [Embed IO, Error AuthenticationError, OwnerRepository, PetRepository] r) =>
  ServerT (TracedAPI OwnersAPI) (Sem r)
ownersHandler =
  insertHandler
    :<|> insertPetHandler
    :<|> updateHandler
    :<|> getHandler
    :<|> searchHandler
    :<|> createPasswordHandler
