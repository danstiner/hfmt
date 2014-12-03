module Language.Haskell.Format (autoSettings, check, checkPath, Stylish.FormatResult (..)) where

import qualified Language.Haskell.Format.HLint   as HLint
import qualified Language.Haskell.Format.Stylish as Stylish

import           Control.Applicative
import           Language.Haskell.HLint3         (Idea)

data Settings = Settings { unHlintSettings :: HLint.Settings, unStylishSettings :: Stylish.Settings }

type CheckResult = ([Idea], Stylish.FormatResult)

autoSettings :: IO Settings
autoSettings = Settings <$> HLint.autoSettings <*> Stylish.autoSettings

checkPath :: Settings -> FilePath -> IO (Either String CheckResult)
checkPath settings path = readFile path >>= check settings (Just path)

check :: Settings -> Maybe FilePath -> String -> IO (Either String CheckResult)
check settings path contents = do
  hlint <- HLint.check (unHlintSettings settings) path contents
  stylish <- Stylish.check (unStylishSettings settings) path contents
  return $ (\a b -> (a,b)) <$> hlint <*> stylish
