module SVet.TH (lookupCompileEnvExp) where

import Data.List (lookup)
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Relude hiding (lift)
import System.Environment

-- | Looks up a compile-time environment variable.
lookupCompileEnv :: String -> String -> Q Text
lookupCompileEnv key defaultValue =
  toText . (?: defaultValue) . lookup key
    <$> runIO getEnvironment

lookupCompileEnvExp :: String -> String -> Q Exp
lookupCompileEnvExp key defaultValue = do
  v <- lookupCompileEnv key defaultValue
  sigE (lift v) [t|Text|]
