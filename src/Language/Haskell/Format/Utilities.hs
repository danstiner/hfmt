module Language.Haskell.Format.Utilities
  ( wasReformatted
  , hunitTest
  , defaultFormatter
  ) where

import           System.IO.Unsafe

import           Language.Haskell.Format
import           Language.Haskell.Format.Definitions
import           Language.Haskell.Source.Enumerator

import           Control.Applicative
import           Control.Monad
import           Data.List
import           Data.Maybe
import           Data.Monoid
import           Pipes
import           Pipes.Parse
import qualified Pipes.Prelude                       as P
import           Test.HUnit

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
  let tests =
        check formatter filepath >-> P.map makeTestCase :: Producer Test IO ()
  TestList <$> evalStateT foldToList tests
  where
    foldToList :: (Monad m) => Parser a m [a]
    foldToList = foldAll (flip (:)) [] id

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
        [ whenMaybe (sourceChanged source reformatted) "Incorrect formatting"
        , whenMaybe
            (hasSuggestions reformatted)
            (concatMap show (suggestions reformatted))
        ]
    whenMaybe :: Bool -> a -> Maybe a
    whenMaybe cond val = const val <$> guard cond

check :: Formatter -> FilePath -> Producer CheckResult IO ()
check formatter filepath =
  enumeratePath filepath >-> P.mapM readSource >->
  P.map (checkFormatting formatter)

readSource :: HaskellSourceFilePath -> IO HaskellSource
readSource filepath = HaskellSource filepath <$> readFile filepath

checkFormatting :: Formatter -> HaskellSource -> CheckResult
checkFormatting (Formatter format) source =
  case format source of
    Left error        -> InvalidCheckResult source error
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
