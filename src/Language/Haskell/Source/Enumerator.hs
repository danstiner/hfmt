{-# LANGUAGE CPP #-}

module Language.Haskell.Source.Enumerator
  ( enumeratePath
  ) where

import           Conduit
import           Control.Applicative
import           Control.Monad
import           Data.List
import           Distribution.PackageDescription
import qualified Distribution.Verbosity                 as Verbosity
import           System.Directory
import           System.FilePath
#if MIN_VERSION_Cabal(2,2,0)
import           Distribution.PackageDescription.Parsec (readGenericPackageDescription)
#elif MIN_VERSION_Cabal(2,0,0)
import           Distribution.PackageDescription.Parse  (readGenericPackageDescription)
#else
import           Distribution.PackageDescription.Parse  (readPackageDescription)

readGenericPackageDescription ::
     Verbosity.Verbosity -> FilePath -> IO GenericPackageDescription
readGenericPackageDescription = readPackageDescription
#endif
enumeratePath :: FilePath -> ConduitT () FilePath IO ()
enumeratePath path = enumPath path .| mapC normalise

enumPath :: FilePath -> ConduitT () FilePath IO ()
enumPath path = do
  isDirectory <- lift $ doesDirectoryExist path
  case isDirectory of
    True -> enumDirectory path
    False
      | hasCabalExtension path -> enumPackage path
    False
      | hasHaskellExtension path -> yield path
    False -> return ()

enumPackage :: FilePath -> ConduitT () FilePath IO ()
enumPackage cabalFile = readPackage cabalFile >>= expandPaths
  where
    readPackage = lift . readGenericPackageDescription Verbosity.silent
    expandPaths = mapM_ (enumPath . mkFull) . sourcePaths
    packageDir = dropFileName cabalFile
    mkFull = (packageDir </>)

enumDirectory :: FilePath -> ConduitT () FilePath IO ()
enumDirectory path = do
  contents <- lift $ getDirectoryContentFullPaths path
  cabalFiles <- lift $ filterM isCabalFile contents
  if null cabalFiles
    then mapM_ enumPath contents
    else mapM_ enumPackage cabalFiles

getDirectoryContentFullPaths :: FilePath -> IO [FilePath]
getDirectoryContentFullPaths path =
  mkFull . notHidden . notMeta <$> getDirectoryContents path
  where
    mkFull = map (path </>)
    notHidden = filter (not . isPrefixOf ".")
    notMeta = (\\ [".", ".."])

isCabalFile :: FilePath -> IO Bool
isCabalFile path = return (hasCabalExtension path) <&&> doesFileExist path

hasCabalExtension :: FilePath -> Bool
hasCabalExtension path = ".cabal" `isSuffixOf` path

hasHaskellExtension :: FilePath -> Bool
hasHaskellExtension path = ".hs" `isSuffixOf` path || ".lhs" `isSuffixOf` path

sourcePaths :: GenericPackageDescription -> [FilePath]
sourcePaths pkg = nub $ concatMap ($ pkg) pathExtractors
  where
    pathExtractors =
      [ maybe [] (hsSourceDirs . libBuildInfo . condTreeData) . condLibrary
      , concatMap (hsSourceDirs . buildInfo . condTreeData . snd) .
        condExecutables
      , concatMap (hsSourceDirs . testBuildInfo . condTreeData . snd) .
        condTestSuites
      , concatMap (hsSourceDirs . benchmarkBuildInfo . condTreeData . snd) .
        condBenchmarks
      ]

(<&&>) :: Applicative f => f Bool -> f Bool -> f Bool
(<&&>) = liftA2 (&&)

infixr 3 <&&> -- same as (&&)
