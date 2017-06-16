module Language.Haskell.Format.Stylish
  ( autoSettings
  , formatter
  ) where

import Control.Applicative
import Language.Haskell.Stylish            as Stylish

import Language.Haskell.Format.Definitions
import Language.Haskell.Format.Internal

newtype Settings =
  Settings Stylish.Config

autoSettings :: IO Settings
autoSettings =
  Settings <$> Stylish.loadConfig (Stylish.makeVerbose False) Nothing

formatter :: Settings -> Formatter
formatter = mkFormatter . stylish

stylish :: Settings -> HaskellSource -> Either String HaskellSource
stylish (Settings config) (HaskellSource source) =
  HaskellSource . unlines <$>
  Stylish.runSteps extensions Nothing steps sourceLines
  where
    sourceLines = lines source
    extensions = Stylish.configLanguageExtensions config
    steps = Stylish.configSteps config
