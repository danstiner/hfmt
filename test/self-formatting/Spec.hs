module Main where

import Language.Haskell.Format.Utilities

import Test.Framework                    (defaultMain, testGroup)
import Test.Framework.Providers.HUnit
import Test.HUnit                        hiding (Test)

main :: IO ()
main =
  defaultMain
    [ testGroup
        "Check formatting of package sources"
        (hUnitTestToTests $ hunitTest "hfmt.cabal")
    ]
