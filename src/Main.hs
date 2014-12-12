{-# LANGUAGE DeriveDataTypeable #-}

import Language.Haskell.Format

import Control.Monad
import Data.Maybe
import System.Console.CmdArgs

data Args = Args {
    onlyPaths :: Bool
  , shouldReplace   :: Bool
  , paths     :: [FilePath]
  } deriving (Show, Data, Typeable)

argsDescription = Args
  {
      onlyPaths = False &= explicit &= name "l" &= help "Do not print reformatted sources or hints to standard output. If a file's formatting is different from hfmt's or has unimplemented hints, print its name to standard output."
    , shouldReplace = False &= explicit &= name "w" &= help "Do not print reformatted sources to standard output. If a file's formatting is different from hfmt's, overwrite it with hfmt's version."
    , paths = [] &= typ "path ..." &= args
  }
  &= summary "Hfmt 0.0.1.0 - A formatter for Haskell programs"
  &= program "hfmt"
  &= details ["Given a file, it operates on that file. By default, hfmt prints the reformatted sources and hints to standard output."]

main :: IO ()
main = do
  args <- cmdArgs argsDescription
  settings <- autoSettings
  run args settings

run :: Args -> Settings -> IO ()
run args settings = mapM_ run' (paths args)
  where
    run' path = checkPath settings path >>= process
    process result = do
      if (shouldReplace args)
        then replace result
        else printResult (onlyPaths args) result

printResult :: Bool -> Either String CheckResult -> IO ()
printResult True = printPath
printResult False = printContents

printPath :: Either String CheckResult -> IO ()
printPath (Left err) = print err
printPath (Right r@(CheckResult mPath _ _)) = when (wasReformatted r) $ putStrLn (fromJust mPath)

printContents :: Either String CheckResult -> IO ()
printContents (Left err) = print err
printContents (Right r@(CheckResult mPath ideas _)) = when (wasReformatted r) $ print r

replace :: Either String CheckResult -> IO ()
replace (Left err) = return ()
replace (Right r@(CheckResult mPath _ _)) = writeFile (fromJust mPath) (formattedResult r)
