import Language.Haskell.Format.Tests  as Hfmt

import Test.Framework                 (defaultMain, testGroup)
import Test.Framework.Providers.HUnit
import Test.HUnit                     hiding (Test)

main :: IO ()
main = defaultMain [
    testGroup "Check source formatting" (hUnitTestToTests $ Hfmt.hunitPackage "hfmt.cabal")
  ]
