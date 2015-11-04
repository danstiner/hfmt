module Language.Haskell.Format.Stylish (autoSettings, formatter) where

import Control.Applicative
import Data.Algorithm.Diff
import Data.Algorithm.DiffContext
import Data.Algorithm.DiffOutput
import Language.Haskell.Stylish            as Stylish
import Text.PrettyPrint

import Language.Haskell.Format.Definitions
import Language.Haskell.Format.Internal

data Settings = Settings Stylish.Config

autoSettings :: IO Settings
autoSettings = Settings <$> Stylish.loadConfig (Stylish.makeVerbose False) Nothing

formatter :: Settings -> Formatter
formatter = mkFormatter . stylish

stylish :: Settings -> HaskellSource -> Either String HaskellSource
stylish (Settings config) (HaskellSource source) =
  HaskellSource . unlines <$> Stylish.runSteps extensions Nothing steps sourceLines
  where
    sourceLines = lines source
    extensions = Stylish.configLanguageExtensions config
    steps = Stylish.configSteps config

showDiff :: FormatResult -> String
showDiff (FormatResult a b) = render (toDoc diff)
  where
    toDoc = prettyContextDiff (text "Original") (text "Reformatted") text
    diff = getContextDiff context (lines a) (lines b)
    context = 1
