module Language.Haskell.Format.HIndent (autoSettings, formatter, defaultFormatter) where

import Control.Applicative
import Data.Maybe
import Data.Text.Lazy                      as L
import Data.Text.Lazy.Builder
import HIndent
import Language.Haskell.Exts.Extension     (Extension)

import Language.Haskell.Format.Definitions
import Language.Haskell.Format.Internal

data Settings = Settings Style (Maybe [Extension])

defaultFormatter :: IO Formatter
defaultFormatter = formatter <$> autoSettings

autoSettings :: IO Settings
autoSettings = return (Settings gibiansky Nothing)

formatter :: Settings -> Formatter
formatter = mkFormatter . hindent

hindent :: Settings -> HaskellSource -> Either String HaskellSource
hindent (Settings style extensions) (HaskellSource source) =
  HaskellSource . L.unpack . toLazyText <$> reformat style extensions sourceText
  where
    sourceText = L.pack source
