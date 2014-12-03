module Language.Haskell.Format.Stylish (check, Settings, FormatResult (..), autoSettings) where

import Control.Applicative
import Language.Haskell.Stylish as Stylish

data FormatResult = FormatResult String String
data Settings = Settings Stylish.Config

autoSettings :: IO Settings
autoSettings = Settings <$> Stylish.loadConfig (Stylish.makeVerbose False) Nothing

check :: Settings -> Maybe FilePath -> String -> IO (Either String FormatResult)
check settings path contents =
    case runResult of
      Left err -> return $ Left err
      Right formattedLines -> ret formattedLines
  where
    runResult = Stylish.runSteps extensions path steps (lines contents)
    ret :: [String] -> IO (Either String FormatResult)
    ret = return . Right . FormatResult contents . unlines
    (Settings config) = settings
    extensions = Stylish.configLanguageExtensions config
    steps = Stylish.configSteps config
