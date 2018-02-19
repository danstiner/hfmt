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
import Data.Bitraversable
import Options.Applicative.Extra          as OptApp
import System.Directory
import System.Exit
import System.IO

main :: IO ()
main = do
  options <- execParser Options.parser
  result <- run options
  exitWith $ exitCode (optAction options) result

run :: Options -> IO RunResult
run opt =
  runConduit $
  sources opt .| mapMC readSource .| mapMC formatSource .| mapMC doAction .|
  foldMapMC toRunResult
  where
    formatSource source = do
      formatter <- defaultFormatter
      return $ applyFormatter formatter source
    doAction :: FormatResult -> IO FormatResult
    doAction = bitraverse return (Actions.act opt)
    toRunResult :: FormatResult -> IO RunResult
    toRunResult (Left err) = do
      hPrint stderr (show err)
      return SourceParseFailure
    toRunResult (Right (Formatted _ source result)) =
      if wasReformatted source result
        then return HadDifferences
        else return NoDifferences

sources :: Options -> Source IO SourceFile
sources opt = lift paths >>= mapM_ sourcesFromPath
  where
    explicitPaths = optPaths opt
    paths =
      if null explicitPaths
        then do
          currentPath <- getCurrentDirectory
          return [currentPath]
        else return explicitPaths

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
