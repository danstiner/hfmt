module Language.Haskell.Format.Utilities
  ( defaultFormatter
  , hunitTest
  , showDiff
  , wasReformatted
  ) where

import System.IO.Unsafe

import Language.Haskell.Format
import Language.Haskell.Source.Enumerator

import Conduit
import Control.Monad
import Data.Algorithm.DiffContext
import Data.List
import Data.Maybe
import Test.HUnit
import Text.PrettyPrint

type ErrorString = String

data CheckResult
  = InvalidCheckResult HaskellSource
                       ErrorString
  | CheckResult HaskellSource
                Reformatted

checkResultPath :: CheckResult -> FilePath
checkResultPath (InvalidCheckResult (HaskellSource filepath _) _) = filepath
checkResultPath (CheckResult (HaskellSource filepath _) _)        = filepath

hunitTest :: FilePath -> Test
hunitTest filepath = TestLabel filepath . unsafePerformIO . testPath $ filepath

testPath :: FilePath -> IO Test
testPath filepath = do
  formatter <- defaultFormatter
  TestList <$>
    runConduit (check formatter filepath .| mapC makeTestCase .| sinkList)

makeTestCase :: CheckResult -> Test
makeTestCase result =
  TestLabel (checkResultPath result) . TestCase $ assertCheckResult result

assertCheckResult :: CheckResult -> IO ()
assertCheckResult result =
  case result of
    (InvalidCheckResult _ errorString) ->
      assertFailure ("Error: " ++ errorString)
    (CheckResult source reformatted) ->
      when (wasReformatted source reformatted) $
      assertFailure (showReformatted source reformatted)
  where
    showReformatted :: HaskellSource -> Reformatted -> String
    showReformatted source reformatted =
      intercalate "\n" $
      catMaybes
        [ showSourceChanges source reformatted
        , showSuggestions source reformatted
        ]
    showSourceChanges source reformatted =
      whenMaybe
        (sourceChanged source reformatted)
        (showDiff source (reformattedSource reformatted))
    showSuggestions _ reformatted =
      whenMaybe
        (hasSuggestions reformatted)
        (concatMap show (suggestions reformatted))
    whenMaybe :: Bool -> a -> Maybe a
    whenMaybe cond val = const val <$> guard cond

showDiff :: HaskellSource -> HaskellSource -> String
showDiff (HaskellSource _ a) (HaskellSource _ b) = render (toDoc diff)
  where
    toDoc = prettyContextDiff (text "Original") (text "Reformatted") text
    diff = getContextDiff linesOfContext (lines a) (lines b)
    linesOfContext = 1

check :: Formatter -> FilePath -> Source IO CheckResult
check formatter filepath =
  enumeratePath filepath .| mapMC readSourceFile .|
  mapC (checkFormatting formatter)

readSourceFile :: FilePath -> IO HaskellSource
readSourceFile filepath = HaskellSource filepath <$> readFile filepath

checkFormatting :: Formatter -> HaskellSource -> CheckResult
checkFormatting (Formatter doFormat) source =
  case doFormat source of
    Left err          -> InvalidCheckResult source err
    Right reformatted -> CheckResult source reformatted

defaultFormatter :: IO Formatter
defaultFormatter = mconcat <$> (autoSettings >>= formatters)

wasReformatted :: HaskellSource -> Reformatted -> Bool
wasReformatted source reformatted =
  hasSuggestions reformatted || sourceChanged source reformatted

sourceChanged :: HaskellSource -> Reformatted -> Bool
sourceChanged source reformatted = source /= reformattedSource reformatted

hasSuggestions :: Reformatted -> Bool
hasSuggestions reformatted = not (null (suggestions reformatted))
