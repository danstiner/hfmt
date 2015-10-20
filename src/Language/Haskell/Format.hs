module Language.Haskell.Format (
    autoSettings,
    check,
    checkPackage,
    checkPath,
    CheckResult(..),
    FormatResult(..),
    formattedResult,
    Settings,
    wasReformatted,
    showDiff,
    showSource,
    checkedPath,
    ) where

import           Control.Applicative
import           Control.Monad
import           Data.List
import           Data.Maybe
import           Distribution.PackageDescription
import           Distribution.PackageDescription.Parse
import qualified Distribution.Verbosity                as Verbosity
import           Language.Haskell.HLint3               (Idea)
import           System.Directory
import           System.FilePath
import           System.FilePath.Glob                  (glob)

import           Language.Haskell.Format.Definitions
import qualified Language.Haskell.Format.HIndent       as HIndent
import qualified Language.Haskell.Format.HLint         as HLint
import qualified Language.Haskell.Format.Stylish       as Stylish

data Settings =
       Settings
         { hlintSettings   :: HLint.Settings
         , stylishSettings :: Stylish.Settings
         , hindentSettings :: HIndent.Settings
         }

data CheckResult = CheckResult (Maybe FilePath) [Idea] FormatResult

instance Show CheckResult where
  show = showDiff

showDiff :: CheckResult -> String
showDiff (CheckResult mPath ideas formatted) =
  fromMaybe "<unknown file>" mPath ++ ":\n" ++
  concatMap show ideas ++
  Stylish.showDiff formatted

showSource :: CheckResult -> String
showSource (CheckResult mPath ideas (FormatResult _ formatted)) =
  fromMaybe "<unknown file>" mPath ++ ":\n" ++
  concatMap show ideas ++
  "\n" ++ formatted

autoSettings :: IO Settings
autoSettings = Settings <$> HLint.autoSettings <*> Stylish.autoSettings <*> HIndent.autoSettings

checkPath :: Settings -> FilePath -> IO [Either String CheckResult]
checkPath settings path = do
  isDir <- doesDirectoryExist path
  if isDir
    then checkDirectory settings path
    else if isCabal
           then checkPackage settings path
           else (: []) <$> checkFile settings path

  where
    isCabal = ".cabal" `isSuffixOf` path

checkDirectory :: Settings -> FilePath -> IO [Either String CheckResult]
checkDirectory settings path = do
  contents <- getDirectoryContentsFullPaths path
  cabalFiles <- filterM isCabalFile contents
  if null cabalFiles
    then concatMapM (checkPath settings) contents
    else concatMapM (checkPath settings) cabalFiles

  where
    getDirectoryContentsFullPaths path = map (path </>) .
                                         filter (not . isPrefixOf ".") .
                                         (\\ [".", ".."]) <$> getDirectoryContents path

concatMapM :: (Monad m) => (a -> m [b]) -> [a] -> m [b]
concatMapM f xs = liftM concat (mapM f xs)

isCabalFile :: FilePath -> IO Bool
isCabalFile path = (hasCabalExtension &&) <$> isFile
  where
    isFile = doesFileExist path
    hasCabalExtension = ".cabal" `isSuffixOf` path

checkFile :: Settings -> FilePath -> IO (Either String CheckResult)
checkFile settings path = readFile path >>= check settings (Just path)

checkPackage :: Settings -> FilePath -> IO [Either String CheckResult]
checkPackage settings pkgPath =
  concat <$> (readPackage pkgPath >>= expandPaths >>= checkPaths)
  where
    readPackage = readPackageDescription Verbosity.silent
    expandPaths = mapM (expandPath . (pkgDir </>)) . sourcePaths
    checkPaths = mapM (checkPath settings) . sources . concat
    pkgDir = dropFileName pkgPath
    sources = filter (\filename -> ".hs" `isSuffixOf` filename || ".lhs" `isSuffixOf` filename)

expandPath :: FilePath -> IO [FilePath]
expandPath filepath = do
  dir <- doesDirectoryExist filepath
  if dir
    then glob (filepath ++ "**/*")
    else return [filepath]

sourcePaths :: GenericPackageDescription -> [FilePath]
sourcePaths pkg = nub . concat $ map ($ pkg) pathExtractors
  where
    pathExtractors = [ maybe [] (hsSourceDirs . libBuildInfo . condTreeData) . condLibrary
                     , concatMap (hsSourceDirs . buildInfo . condTreeData . snd) . condExecutables
                     , concatMap (hsSourceDirs . testBuildInfo . condTreeData . snd) . condTestSuites
                     , concatMap (hsSourceDirs . benchmarkBuildInfo . condTreeData . snd) . condBenchmarks
                     ]

check :: Settings -> Maybe FilePath -> String -> IO (Either String CheckResult)
check settings path contents = do
  hlint <- HLint.check (hlintSettings settings) path contents
  let hindent = HIndent.check (hindentSettings settings) path contents
  stylish <- either
               (return . Left)
               (\(FormatResult _ contents') ->
                  Stylish.check (stylishSettings settings) path contents')
               hindent
  let formatted = (\(FormatResult _ contents'') -> FormatResult contents contents'') <$> stylish
  return $ CheckResult (fmap normalise path) <$> hlint <*> formatted

wasReformatted :: CheckResult -> Bool
wasReformatted (CheckResult _ ideas (FormatResult before after)) =
  not (null ideas) || before /= after

checkedPath :: CheckResult -> Maybe FilePath
checkedPath (CheckResult mPath _ _) = mPath

formattedResult :: CheckResult -> String
formattedResult (CheckResult _ _ (FormatResult _ after)) = after
