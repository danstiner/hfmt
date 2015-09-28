module Language.Haskell.Format.HIndent (autoSettings, check, Settings) where

import Language.Haskell.Format.Definitions

import Control.Applicative
import Data.Maybe
import Data.Text.Lazy                      as L
import Data.Text.Lazy.Builder
import HIndent
import Language.Haskell.Exts.Extension     (Extension)

data Settings = Settings Style (Maybe [Extension])

autoSettings :: IO Settings
autoSettings = return (Settings gibiansky Nothing)

check :: Settings -> Maybe FilePath -> String -> Either String FormatResult
check (Settings style extensions) _ contents = FormatResult contents . L.unpack . toLazyText <$> reformat
                                                                                                   style
                                                                                                   extensions
                                                                                                   (L.pack
                                                                                                      contents)
