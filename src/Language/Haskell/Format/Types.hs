module Language.Haskell.Format.Types
  ( HaskellSource(..)
  , Reformatted(..)
  , Formatter(..)
  , Suggestion(..)
  ) where

import Control.Monad
import Data.Monoid   ((<>))

type ErrorString = String

data HaskellSource =
  HaskellSource FilePath
                String
  deriving (Eq)

newtype Suggestion =
  Suggestion String

instance Show Suggestion where
  show (Suggestion text) = text

data Reformatted = Reformatted
  { reformattedSource :: HaskellSource
  , suggestions       :: [Suggestion]
  }

instance Monoid Reformatted where
  mempty = Reformatted undefined []
  (Reformatted _ suggestionsA) `mappend` (Reformatted sourceB suggestionsB) =
    Reformatted sourceB (suggestionsA <> suggestionsB)

newtype Formatter = Formatter
  { unFormatter :: HaskellSource -> Either ErrorString Reformatted
  }

instance Monoid Formatter where
  mempty = Formatter (\source -> Right (Reformatted source []))
  (Formatter f) `mappend` (Formatter g) = Formatter (asReformatter g <=< f)

asReformatter ::
     (HaskellSource -> Either ErrorString Reformatted)
  -> Reformatted
  -> Either ErrorString Reformatted
asReformatter formatter r = (r <>) <$> formatter (reformattedSource r)
