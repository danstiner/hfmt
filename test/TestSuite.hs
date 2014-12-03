import Language.Haskell.Format.Tests  as Hfmt

import Test.Framework                 (defaultMain, testGroup)
import Test.Framework.Providers.HUnit
import Test.HUnit                     hiding (Test)

main :: IO ()
main = do
  src <- hunitGlob "src/**/*.hs"
  test <- hunitGlob "test/**/*.hs"
  defaultMain [
      testGroup "Validate Formatting in src/**/*.hs" (hUnitTestToTests src)
    , testGroup "Validate Formatting in test/**/*.hs" (hUnitTestToTests test)
    ]
