module Language.Haskell.Format.Internal
  ( mkFormatter
  , mkSuggester
  ) where

import Language.Haskell.Format.Definitions

import Control.Applicative

mkFormatter :: (HaskellSource -> Either String HaskellSource) -> Formatter
mkFormatter f = Formatter (fmap (\source -> Reformatted source []) . f)

mkSuggester :: (HaskellSource -> Either String [Suggestion]) -> Formatter
mkSuggester f = Formatter $ \source -> Reformatted source <$> f source
