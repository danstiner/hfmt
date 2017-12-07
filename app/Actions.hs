module Actions
  ( act
  ) where

import Language.Haskell.Format
import Language.Haskell.Format.Utilities
import OptionsParser
import Types

import Control.Monad

act :: Options -> ReformatResult -> IO ReformatResult
act options r@(InvalidReformat input errorString) = do
  putStrLn ("Error reformatting " ++ show input ++ ": " ++ errorString)
  return r
act options r@(Reformat input source result) = act' (optAction options)
  where
    act' PrintDiffs =
      when wasReformatted (printDiff input source result) >> return r
    act' PrintSources = do
      when wasReformatted (printSource $ reformattedSource result)
      return (Reformat input (reformattedSource result) result)
    act' PrintFilePaths = when wasReformatted (print input) >> return r
    act' WriteSources = do
      when wasReformatted (writeSource input (reformattedSource result))
      return (Reformat input (reformattedSource result) result)
    wasReformatted = sourceChangedOrHasSuggestions source result

sourceChangedOrHasSuggestions :: HaskellSource -> Reformatted -> Bool
sourceChangedOrHasSuggestions source reformatted =
  not (null (suggestions reformatted)) ||
  source /= reformattedSource reformatted

printDiff :: InputFile -> HaskellSource -> Reformatted -> IO ()
printDiff (InputFilePath path) source reformatted = do
  putStrLn (path ++ ":")
  mapM_ (putStr . show) (suggestions reformatted)
  putStr (showDiff source (reformattedSource reformatted))
printDiff InputFromStdIn source reformatted = do
  mapM_ (putStr . show) (suggestions reformatted)
  putStr (showDiff source (reformattedSource reformatted))

printSource :: HaskellSource -> IO ()
printSource (HaskellSource _ source) = putStr source

writeSource :: InputFile -> HaskellSource -> IO ()
writeSource (InputFilePath path) (HaskellSource _ source) =
  writeFile path source
writeSource InputFromStdIn (HaskellSource _ source) = putStr source
