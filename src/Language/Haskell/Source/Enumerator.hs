module Language.Haskell.Source.Enumerator (enumeratePath, HaskellSourceFilePath) where

import           Control.Applicative
import           Control.Monad
import           Data.List
import           Distribution.PackageDescription
import           Distribution.PackageDescription.Parse
import qualified Distribution.Verbosity                as Verbosity
import           Pipes
import           System.Directory
import           System.FilePath

type HaskellSourceFilePath = FilePath

enumeratePath :: FilePath -> Producer HaskellSourceFilePath IO ()
enumeratePath = filePath

filePath :: FilePath -> Producer HaskellSourceFilePath IO ()
filePath path = do
  isCabal <- lift $ isCabalFile path
  if isCabal
    then package' path
    else directoryOrFile path

simpleFilePath :: FilePath -> Producer HaskellSourceFilePath IO ()
simpleFilePath path = do
  isHaskellSource <- lift $ isHaskellSourceFile path
  when isHaskellSource $ yield path

simpleDirectory :: FilePath -> Producer HaskellSourceFilePath IO ()
simpleDirectory path = do
  contents <- lift $ getDirectoryContentsFullPaths path
  mapM_ simpleFileOrDirectory contents

simpleFileOrDirectory :: FilePath -> Producer HaskellSourceFilePath IO ()
simpleFileOrDirectory filepath = do
  dir <- lift $ doesDirectoryExist filepath
  if dir
    then simpleDirectory filepath
    else simpleFilePath filepath

package' :: FilePath -> Producer HaskellSourceFilePath IO ()
package' path = readPackage path >>= expandPaths
  where
    readPackage = lift . readPackageDescription Verbosity.silent
    expandPaths = mapM_ (simpleFileOrDirectory . mkFull) . sourcePaths
    dir = dropFileName path
    mkFull = (dir </>)

directory :: FilePath -> Producer HaskellSourceFilePath IO ()
directory path = do
  contents <- lift $ getDirectoryContentsFullPaths path
  cabalFiles <- lift $ filterM isCabalFile contents
  if null cabalFiles
    then mapM_ directoryOrFile contents
    else mapM_ package' cabalFiles

directoryOrFile :: FilePath -> Producer HaskellSourceFilePath IO ()
directoryOrFile path = do
  isFile <- lift $ doesFileExist path
  if isFile
    then simpleFilePath path
    else directory path

getDirectoryContentsFullPaths :: FilePath -> IO [FilePath]
getDirectoryContentsFullPaths path = mkFull . notHidden . notMeta <$> getDirectoryContents path
  where
    mkFull = map (path </>)
    notHidden = filter (not . isPrefixOf ".")
    notMeta = (\\ [".", ".."])

isCabalFile :: FilePath -> IO Bool
isCabalFile path = (hasCabalExtension &&) <$> isFile
  where
    isFile = doesFileExist path
    hasCabalExtension = ".cabal" `isSuffixOf` path

isHaskellSourceFile :: FilePath -> IO Bool
isHaskellSourceFile path = (hasHaskellExtension &&) <$> isFile
  where
    isFile = doesFileExist path
    hasHaskellExtension = ".hs" `isSuffixOf` path || ".lhs" `isSuffixOf` path

sourcePaths :: GenericPackageDescription -> [FilePath]
sourcePaths pkg = nub $ concatMap ($ pkg) pathExtractors
  where
    pathExtractors = [ maybe [] (hsSourceDirs . libBuildInfo . condTreeData) . condLibrary
                     , concatMap (hsSourceDirs . buildInfo . condTreeData . snd) . condExecutables
                     , concatMap (hsSourceDirs . testBuildInfo . condTreeData . snd) . condTestSuites
                     , concatMap (hsSourceDirs . benchmarkBuildInfo . condTreeData . snd) . condBenchmarks
                     ]
