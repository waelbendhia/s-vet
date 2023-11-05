module Main ( main ) where

import           Relude     hiding ( runReader )

import           Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = pass
