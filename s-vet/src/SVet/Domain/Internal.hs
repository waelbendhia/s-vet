{-# LANGUAGE AllowAmbiguousTypes #-}

module SVet.Domain.Internal (
  WithField (..),
  Entity,
  DomainEntity,
  AsEntity (..),
  fromSeldaID,
  toSeldaID,
  EntKey,
  (<.&&>),
  throwIfNoRows,
  note,
  justOne,
) where

import Data.Aeson hiding (Result)
import qualified Data.Aeson.Key as AK
import Data.Aeson.Optics
import Database.Selda hiding (toText)
import GHC.Exts
import GHC.TypeLits
import Optics
import Polysemy
import Polysemy.Error
import Relude
import Servant

data WithField (l :: Symbol) v d = WithField {field :: v, wrapped :: d}
  deriving (Eq, Show)

instance
  {-# OVERLAPPING #-}
  (a ~ v, b ~ v, k ~ A_Lens) =>
  LabelOptic symbol k (WithField symbol v r) (WithField symbol v r) a b
  where
  labelOptic = lensVL $ \f (WithField v d) -> (`WithField` d) <$> f v

instance
  {-# OVERLAPPING #-}
  (k ~ A_Lens, LabelOptic' l k d dt, a ~ dt, b ~ dt) =>
  LabelOptic
    l
    k
    ( WithField
        field
        fieldValue
        d
    )
    (WithField field fieldValue d)
    a
    b
  where
  labelOptic =
    lens
      (\wf -> wrapped wf ^. fromLabel @l)
      (\wf v -> wf{wrapped = wrapped wf & fromLabel @l .~ v})

instance
  (FromJSON v, FromJSON r, KnownSymbol f) =>
  FromJSON (WithField f v r)
  where
  parseJSON v =
    WithField
      <$> withObject
        ("WithField " <> fieldName <> " ")
        (.: fromString fieldName)
        v
      <*> parseJSON v
   where
    fieldName = symbolVal (Proxy @f)

instance (ToJSON v, ToJSON r, KnownSymbol f) => ToJSON (WithField f v r) where
  toJSON (WithField k e) = toJSON e & _Object % at (AK.fromText fieldName) ?~ toJSON k
   where
    fieldName = toText $ symbolVal (Proxy @f)

newtype EntKey e = EntKey Int64
  deriving (Show, Eq, ToJSON, FromJSON, Typeable, FromHttpApiData, Ord)

type family DomainEntity selda

class AsEntity seldaj where
  asEntity :: seldaj -> Entity (DomainEntity seldaj)

fromSeldaID :: ID selda -> EntKey (DomainEntity selda)
fromSeldaID = EntKey . fromId

toSeldaID :: EntKey (DomainEntity selda) -> ID selda
toSeldaID = toId . coerce

type Entity e = WithField "key" (EntKey e) e

(<.&&>) ::
  forall (s :: Type) (t :: Type) f.
  (Applicative f, Same s t) =>
  f (Col s Bool) ->
  f (Col t Bool) ->
  f (Col s Bool)
a <.&&> b = (.&&) @s @t <$> a <*> b

infixr 3 <.&&>

justOne ::
  forall error r a.
  Members '[Error error] r =>
  error ->
  [a] ->
  Sem r a
justOne e = note e . listToMaybe

throwIfNoRows ::
  forall error r a.
  (MonadSelda (Sem r), Result a, Members '[Error error] r) =>
  error ->
  Query (Backend (Sem r)) a ->
  Sem r (Res a)
throwIfNoRows err = justOne err <=< query
