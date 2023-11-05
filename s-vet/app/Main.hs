module Main where

import SVet
import Options.Generic
import Relude

main :: IO ()
main = startApp =<< getRecord "SVet Server"
