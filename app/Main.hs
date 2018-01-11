{-# LANGUAGE Rank2Types #-}

module Main
  ( main
  ) where

import Actions
import ExitCode
import Language.Haskell.Format
import Language.Haskell.Format.Utilities
import Language.Haskell.Source.Enumerator
import Options
import Types

import Conduit
import Options.Applicative.Extra          as OptApp
import System.Directory
import System.Exit

main :: IO ()
main = do
  options <- execParser Options.parser
  formattedCodeDiffers <- run options
  exitWith $ exitCode (optAction options) formattedCodeDiffers

run :: Options -> IO Bool
run options =
  runConduit $
  sources .| mapMC readSource .| mapMC formatSource .| handleFormatErrors .|
  mapMC action .|
  summarize
  where
    paths = do
      let explicitPaths = optPaths options
      if null explicitPaths
        then do
          currentPath <- getCurrentDirectory
          return [currentPath]
        else return explicitPaths
    sources :: Source IO SourceFile
    sources = lift paths >>= mapM_ sourcesFromPath
    formatSource source = do
      formatter <- defaultFormatter
      return $ applyFormatter formatter source
    handleFormatErrors = concatMapMC handleFormatError
    action = Actions.act options
    summarize =
      anyC' (\(Formatted _ source result) -> wasReformatted source result)

sourcesFromPath :: FilePath -> Source IO SourceFile
sourcesFromPath "-"  = yield StdinSource
sourcesFromPath path = enumeratePath path .| mapC SourceFilePath

readSource :: SourceFile -> IO SourceFileWithContents
readSource s@(SourceFilePath path) =
  SourceFileWithContents s . HaskellSource path <$> readFile path
readSource s@StdinSource =
  SourceFileWithContents s . HaskellSource "stdin" <$> getContents

applyFormatter :: Formatter -> SourceFileWithContents -> FormatResult
applyFormatter (Formatter doFormat) (SourceFileWithContents file contents) =
  case doFormat contents of
    Left err       -> Left (FormatError file err)
    Right reformat -> Right (Formatted file contents reformat)

handleFormatError :: FormatResult -> IO (Maybe Formatted)
handleFormatError (Left err)        = printFormatError err >> return Nothing
handleFormatError (Right formatted) = return $ Just formatted

printFormatError :: FormatError -> IO ()
printFormatError (FormatError input errorString) =
  putStrLn ("Error reformatting " ++ show input ++ ": " ++ errorString)

-- | Check that at least one value in the stream returns True.
--
-- Does not shortcut, entire stream is always consumed
anyC' :: Monad m => (a -> Bool) -> Consumer a m Bool
anyC' f = do
  result <- anyC f -- Check for at least one value, may shortcut
  sinkNull -- consume any remaining input skipped by a shortcut
  return result
