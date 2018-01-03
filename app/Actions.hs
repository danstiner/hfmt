module Actions
  ( act
  ) where

import Language.Haskell.Format
import Language.Haskell.Format.Utilities
import Options
import Types

import Control.Monad

act :: Options -> Formatted -> IO Formatted
act options r@(Formatted input source result) = act' (optAction options)
  where
    act' PrintDiffs =
      when wasReformatted' (printDiff input source result) >> return r
    act' PrintSources = do
      when wasReformatted' (printSource $ reformattedSource result)
      return (Formatted input (reformattedSource result) result)
    act' PrintFilePaths = when wasReformatted' (print input) >> return r
    act' WriteSources = do
      when wasReformatted' (writeSource input (reformattedSource result))
      return (Formatted input (reformattedSource result) result)
    wasReformatted' = wasReformatted source result

printDiff :: SourceFile -> HaskellSource -> Reformatted -> IO ()
printDiff (SourceFilePath path) source reformatted = do
  putStrLn (path ++ ":")
  mapM_ (putStr . show) (suggestions reformatted)
  putStr (showDiff source (reformattedSource reformatted))
printDiff StdinSource source reformatted = do
  mapM_ (putStr . show) (suggestions reformatted)
  putStr (showDiff source (reformattedSource reformatted))

printSource :: HaskellSource -> IO ()
printSource (HaskellSource _ source) = putStr source

writeSource :: SourceFile -> HaskellSource -> IO ()
writeSource (SourceFilePath path) (HaskellSource _ source) =
  writeFile path source
writeSource StdinSource (HaskellSource _ source) = putStr source
