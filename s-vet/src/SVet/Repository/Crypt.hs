{-# LANGUAGE BlockArguments #-}

module SVet.Repository.Crypt where

import Crypto.BCrypt
import Polysemy
import Polysemy.Error
import Relude hiding (MonadReader, Reader, ask)

data HashingError = HashingFailed
  deriving (Show, Eq)

instance Exception HashingError

data Crypt m a where
  Hash :: Text -> Crypt m ByteString
  CompareToHash :: Text -> ByteString -> Crypt m Bool

makeSem ''Crypt

runCrypt ::
  (Members '[Error HashingError, Embed IO] r) =>
  HashingPolicy ->
  Sem (Crypt : r) a ->
  Sem r a
runCrypt policy = interpret \case
  Hash s -> do
    res <- embed (hashPasswordUsingPolicy policy $ encodeUtf8 s)
    maybe (throw HashingFailed) pure res
  CompareToHash p s -> pure $ validatePassword s (encodeUtf8 p)
