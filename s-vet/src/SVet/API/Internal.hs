{-# LANGUAGE AllowAmbiguousTypes #-}

module SVet.API.Internal (
  mustBeVet,
  mustBeOwner,
  mustBeAuthenticated,
  authenticateVet,
  authenticateOwner,
  authenticate,
  SVetAuth,
  throwAllErr,
  serverWithVet,
  serverWithAccount,
) where

import Polysemy
import Polysemy.Error
import Polysemy.Reader
import Relude hiding (Reader, runReader)
import SVet.Authentication
import SVet.Domain
import Servant
import Servant.Auth.Server

mustBeVet ::
  Members '[Error AuthenticationError] r =>
  AuthResult Account ->
  Sem r Email
mustBeVet (Authenticated (VetAccount email')) = pure email'
mustBeVet _ = throw UnauthorizedAccess

readFromAuth ::
  forall (api :: Type) v r.
  (HasServer api '[]) =>
  (AuthResult Account -> Sem r v) ->
  AuthResult Account ->
  ServerT api (Sem (Reader v : r)) ->
  ServerT api (Sem r)
readFromAuth p auth =
  hoistServer @api @(Sem (Reader v : r)) @(Sem r)
    Proxy
    (\a -> join $ runReader <$> p auth <*> pure a)

authenticateVet ::
  forall (api :: Type) r.
  (Members '[Error AuthenticationError] r, HasServer api '[]) =>
  AuthResult Account ->
  ServerT api (Sem (Reader Email : r)) ->
  ServerT api (Sem r)
authenticateVet = readFromAuth @api @_ @r mustBeVet

mustBeOwner ::
  Members '[Error AuthenticationError] r =>
  AuthResult Account ->
  Sem r (EntKey Owner)
mustBeOwner (Authenticated (OwnerAccount k)) = pure k
mustBeOwner _ = throw UnauthorizedAccess

authenticateOwner ::
  forall (api :: Type) r.
  (Members '[Error AuthenticationError] r, HasServer api '[]) =>
  AuthResult Account ->
  ServerT api (Sem (Reader (EntKey Owner) : r)) ->
  ServerT api (Sem r)
authenticateOwner = readFromAuth @api @_ @r mustBeOwner

mustBeAuthenticated ::
  Members '[Error AuthenticationError] r =>
  AuthResult Account ->
  Sem r (Either (EntKey Owner) Email)
mustBeAuthenticated (Authenticated (VetAccount email')) = pure $ Right email'
mustBeAuthenticated (Authenticated (OwnerAccount k)) = pure $ Left k
mustBeAuthenticated _ = throw UnauthorizedAccess

authenticate ::
  forall (api :: Type) r.
  (Members '[Error AuthenticationError] r, HasServer api '[]) =>
  AuthResult Account ->
  ServerT api (Sem (Reader (Either (EntKey Owner) Email) : r)) ->
  ServerT api (Sem r)
authenticate = readFromAuth @api @_ @r mustBeAuthenticated

type SVetAuth = Auth '[JWT, Cookie] Account

throwAllErr ::
  forall (e :: Type) (api :: Type) r.
  (Members '[Error e] r, HasServer api '[], ThrowAll (ServerT api Handler)) =>
  e ->
  ServerT api (Sem r)
throwAllErr e = hoistServer @api @Handler @(Sem r) Proxy (const $ throw e) (throwAll err500)

serverWithVet ::
  forall api r.
  (Members '[Error AuthenticationError] r, HasServer api '[], ThrowAll (ServerT api Handler)) =>
  (Email -> ServerT api (Sem r)) ->
  ServerT (SVetAuth :> api) (Sem r)
serverWithVet s (Authenticated (VetAccount v)) = s v
serverWithVet _ _ = throwAllErr @_ @api @r UnauthorizedAccess

serverWithAccount ::
  forall api r.
  (Members '[Error AuthenticationError] r, HasServer api '[], ThrowAll (ServerT api Handler)) =>
  (Either (EntKey Owner) Email -> ServerT api (Sem r)) ->
  ServerT (SVetAuth :> api) (Sem r)
serverWithAccount s (Authenticated (VetAccount v)) = s $ Right v
serverWithAccount s (Authenticated (OwnerAccount k)) = s $ Left k
serverWithAccount _ _ = throwAllErr @_ @api @r UnauthorizedAccess
