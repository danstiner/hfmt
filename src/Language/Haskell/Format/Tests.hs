module Language.Haskell.Format.Tests (
    hunit
  , hunitGlob
  , hunitPackage
) where

import           Control.Monad
import           Data.List
import           Data.Maybe
import           Distribution.PackageDescription
import           Distribution.PackageDescription.Configuration
import           Distribution.PackageDescription.Parse
import qualified Distribution.Verbosity                        as Verbosity
import           Language.Haskell.Format                       as Format
import           System.Directory
import           System.FilePath.Glob                          (glob)
import           Test.HUnit

type FilePathGlob = String

hunit :: [FilePath] -> Test
hunit = TestList . map hunitTestcase

hunitGlob :: FilePathGlob -> IO Test
hunitGlob pattern = fmap (TestList . map hunitTestcase) (glob pattern)

hunitPackage :: FilePath -> Test
hunitPackage path = TestCase $ do
  genericPkg <- readPackageDescription Verbosity.silent path
  settings <- Format.autoSettings
  files <- mapM expandPath $ sourcePaths genericPkg
  results <- mapM (Format.checkPath settings) . sources $ concat files
  mapM_ assertResult results
  where
    sources = filter (\path -> ".hs" `isSuffixOf` path || ".lhs" `isSuffixOf` path)

assertResult :: Either String CheckResult -> IO ()
assertResult r = case r of
  Left err -> assertFailure ("Parse error: " ++ err)
  Right (CheckResult mPath ideas formatted) -> if not (null ideas)
      then assertFailure $ concatMap show ideas
      else assertOnFormatting formatted (fromMaybe "(unknown)" mPath)
  where
    assertOnFormatting (FormatResult before after) path =
      when (before /= after) $ assertFailure ("Bad formatting in " ++ path)

unsupportedReason :: PackageDescription -> Maybe String
unsupportedReason package = case buildType package of
  Just Simple -> Nothing
  Just Configure -> Nothing
  Just bt -> Just ("Unsupported build type: " ++ show bt)
  Nothing -> Just "Build type not found"

expandPath :: FilePath -> IO [FilePath]
expandPath path = do
  dir <- doesDirectoryExist path
  if dir
    then glob (path ++ "**/*")
    else return [path]

sourcePaths :: GenericPackageDescription -> [FilePath]
sourcePaths pkg = nub . concat $ map ($ pkg) extractors
  where
    extractors = [
        maybe [] (hsSourceDirs . libBuildInfo . condTreeData) . condLibrary
      , concatMap (hsSourceDirs . buildInfo . condTreeData . snd) . condExecutables
      , concatMap (hsSourceDirs . testBuildInfo . condTreeData . snd) . condTestSuites
      , concatMap (hsSourceDirs . benchmarkBuildInfo . condTreeData . snd) . condBenchmarks
      ]

hunitTestcase :: FilePath -> Test
hunitTestcase path = TestLabel path . TestCase $ do
  settings <- Format.autoSettings
  result <- Format.checkPath settings path
  assertResult result
