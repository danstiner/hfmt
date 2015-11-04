module Language.Haskell.Format.Utilities (wasReformatted, hunitTest, defaultFormatter) where

import           Language.Haskell.Format
import           Language.Haskell.Format.Definitions
import           Language.Haskell.Source.Enumerator

import           Control.Applicative
import           Control.Monad
import           Data.Monoid
import           Pipes
import qualified Pipes.Prelude                       as P
import           Test.HUnit

type ErrorString = String

data CheckResult = InvalidCheckResult ErrorString
                 | CheckResult HaskellSource Reformatted

hunitTest :: FilePath -> Test
hunitTest filepath = TestLabel filepath $ makeTestCase filepath

makeTestCase :: FilePath -> Test
makeTestCase filepath = TestCase $ do
  formatter <- defaultFormatter
  runEffect $ check formatter filepath >-> assertResults

assertResults :: Consumer CheckResult IO ()
assertResults = forever $ do
  result <- await
  case result of
    (InvalidCheckResult errorString) -> lift $ assertFailure ("Error: " ++ errorString)
    (CheckResult source reformatted) -> when (wasReformatted source reformatted) $ lift $ assertFailure
                                                                                            ("Incorrect formatting: " ++ concatMap
                                                                                                                           show
                                                                                                                           (suggestions
                                                                                                                              reformatted))

check :: Formatter -> FilePath -> Producer CheckResult IO ()
check formatter path = enumeratePath path >->
                       P.mapM readSource >->
                       P.map (checkFormatting formatter)

readSource :: HaskellSourceFilePath -> IO HaskellSource
readSource path = HaskellSource <$> readFile path

checkFormatting :: Formatter -> HaskellSource -> CheckResult
checkFormatting (Formatter format) source =
  case format source of
    Left error        -> InvalidCheckResult error
    Right reformatted -> CheckResult source reformatted

fix :: Formatter -> FilePath -> IO ()
fix = undefined

defaultFormatter :: IO Formatter
defaultFormatter = mconcat <$> (autoSettings >>= formatters)

wasReformatted :: HaskellSource -> Reformatted -> Bool
wasReformatted source reformatted =
  not (null (suggestions reformatted)) || source /= reformattedSource reformatted
