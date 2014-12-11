module Language.Haskell.Format (Settings, autoSettings, check, checkPath, CheckResult (..), Stylish.FormatResult (..), wasReformatted, formattedResult) where

import qualified Language.Haskell.Format.HLint   as HLint
import qualified Language.Haskell.Format.Stylish as Stylish

import           Control.Applicative
import           Language.Haskell.HLint3         (Idea)
import           System.Directory

data Settings = Settings { unHlintSettings :: HLint.Settings, unStylishSettings :: Stylish.Settings }

data CheckResult = CheckResult (Maybe FilePath) [Idea] Stylish.FormatResult

instance Show CheckResult where
  show (CheckResult mPath ideas formatted) =
    show mPath ++ concatMap show ideas ++ Stylish.showDiff formatted

autoSettings :: IO Settings
autoSettings = Settings <$> HLint.autoSettings <*> Stylish.autoSettings

checkPath :: Settings -> FilePath -> IO (Either String CheckResult)
checkPath settings path = do
  isDir <- doesDirectoryExist path
  if isDir
    then return (Left $ path ++ " is a directory")
    else readFile path >>= check settings (Just path)

check :: Settings -> Maybe FilePath -> String -> IO (Either String CheckResult)
check settings path contents = do
  hlint <- HLint.check (unHlintSettings settings) path contents
  stylish <- Stylish.check (unStylishSettings settings) path contents
  return $ CheckResult path <$> hlint <*> stylish

wasReformatted :: CheckResult -> Bool
wasReformatted (CheckResult _ ideas (Stylish.FormatResult before after)) = not (null ideas) || before /= after

formattedResult :: CheckResult -> String
formattedResult (CheckResult _ _ (Stylish.FormatResult _ after)) = after
