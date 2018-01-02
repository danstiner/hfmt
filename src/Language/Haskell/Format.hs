module Language.Haskell.Format
  ( autoSettings
  , format
  , formatters
  , hlint
  , hindent
  , stylish
  , Settings
  , Formatter(..)
  , Suggestion(..)
  , HaskellSource(..)
  , Reformatted(..)
  ) where

import qualified Language.Haskell.Format.HIndent as HIndent
import qualified Language.Haskell.Format.HLint   as HLint
import qualified Language.Haskell.Format.Stylish as Stylish
import           Language.Haskell.Format.Types

data Settings =
  Settings

autoSettings :: IO Settings
autoSettings = return Settings

hlint :: Settings -> IO Formatter
hlint _ = HLint.suggester <$> HLint.autoSettings

hindent :: Settings -> IO Formatter
hindent _ = HIndent.formatter <$> HIndent.autoSettings

stylish :: Settings -> IO Formatter
stylish _ = Stylish.formatter <$> Stylish.autoSettings

formatters :: Settings -> IO [Formatter]
formatters s = sequence [hlint s, hindent s, stylish s]

format :: Formatter -> HaskellSource -> Either String Reformatted
format = unFormatter
