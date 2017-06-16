module Main
  ( main
  ) where

import Test.Framework                 (defaultMain, testGroup)
import Test.Framework.Providers.HUnit
import Test.HUnit                     hiding (Test)

main :: IO ()
main = defaultMain []
