module Language.Haskell.Format.HLint (check, Settings, autoSettings) where

import           Data.Maybe
import           Language.Haskell.HLint3 (Classify, Hint, Idea, ParseFlags)
import qualified Language.Haskell.HLint3 as HLint

data Settings = Settings ParseFlags [Classify] Hint

autoSettings :: IO Settings
autoSettings = do
  (parseFlags, classifications, hint) <- HLint.autoSettings
  return (Settings parseFlags classifications hint)

check :: Settings -> Maybe FilePath -> String -> IO (Either String [Idea])
check settings mPath contents = do
  parsed <- HLint.parseModuleEx parseFlags (fromMaybe "" mPath) (Just contents)
  return $ case parsed of
    Left parseError -> Left (showParseError parseError)
    Right parseResult -> Right $ HLint.applyHints classifications hint [parseResult]
  where
    Settings parseFlags classifications hint = settings

showParseError :: HLint.ParseError -> String
showParseError = HLint.parseErrorMessage
