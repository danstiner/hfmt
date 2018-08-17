module Language.Haskell.Format.Stylish
  ( autoSettings
  , formatter
  ) where

import Language.Haskell.Stylish         as Stylish

import Language.Haskell.Format.Internal
import Language.Haskell.Format.Types

newtype Settings =
  Settings Stylish.Config

autoSettings :: IO Settings
autoSettings = do
  path <- Stylish.configFilePath verbose Nothing
  config <- Stylish.loadConfig verbose path
  return (Settings config)
  where
    verbose = Stylish.makeVerbose False

formatter :: Settings -> Formatter
formatter = mkFormatter . stylish

stylish :: Settings -> HaskellSource -> Either String HaskellSource
stylish (Settings config) (HaskellSource filepath source) =
  HaskellSource filepath . unlines <$>
  Stylish.runSteps extensions (Just filepath) steps sourceLines
  where
    sourceLines = lines source
    extensions = Stylish.configLanguageExtensions config
    steps = Stylish.configSteps config
