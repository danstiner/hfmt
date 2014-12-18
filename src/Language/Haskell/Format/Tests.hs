module Language.Haskell.Format.Tests
    (
      hunit
    , hunitGlob
    , hunitPackage
    ) where

import Control.Applicative
import Control.Monad
import Language.Haskell.Format as Format
import System.FilePath.Glob    (glob)
import Test.HUnit

type FilePathGlob = String

hunit :: FilePath -> Test
hunit = hunitTestcase

hunitGlob :: FilePathGlob -> IO Test
hunitGlob pattern = TestList . map hunitTestcase <$> glob pattern

hunitPackage :: FilePath -> Test
hunitPackage = makeTestCase . flip checkPackage

assertResults :: [Either String CheckResult] -> IO ()
assertResults = mapM_ assertResult

assertResult :: Either String CheckResult -> IO ()
assertResult r = case r of
  Left err -> assertFailure ("Parse error: " ++ err)
  Right result -> when (wasReformatted result) $ assertFailure ("Incorrect formatting: " ++ show result)

hunitTestcase :: FilePath -> Test
hunitTestcase filepath = TestLabel filepath $ makeTestCase (`checkPath` filepath)

makeTestCase :: (Settings -> IO [Either String CheckResult]) -> Test
makeTestCase checkFunc =
    TestCase $ autoSettings >>= checkFunc >>= assertResults
