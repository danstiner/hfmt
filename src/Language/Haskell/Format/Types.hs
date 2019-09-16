module Language.Haskell.Format.Types
  ( HaskellSource(..)
  , Reformatted(..)
  , Formatter(..)
  , Suggestion(..)
  ) where

import Control.Monad
import Data.Semigroup (Semigroup, (<>))

type ErrorString = String

data HaskellSource =
  HaskellSource FilePath String
  deriving (Eq)

newtype Suggestion =
  Suggestion String

instance Show Suggestion where
  show (Suggestion text) = text

data Reformatted =
  Reformatted
    { reformattedSource :: HaskellSource
    , suggestions       :: [Suggestion]
    }

instance Semigroup Reformatted where
  (Reformatted _ suggestionsA) <> (Reformatted sourceB suggestionsB) =
    Reformatted sourceB (suggestionsA <> suggestionsB)

instance Monoid Reformatted where
  mempty = Reformatted undefined []
  mappend = (<>)

newtype Formatter =
  Formatter
    { unFormatter :: HaskellSource -> Either ErrorString Reformatted
    }

instance Semigroup Formatter where
  (Formatter f) <> (Formatter g) = Formatter (asReformatter g <=< f)

instance Monoid Formatter where
  mempty = Formatter (\source -> Right (Reformatted source []))
  mappend = (<>)

asReformatter ::
     (HaskellSource -> Either ErrorString Reformatted)
  -> Reformatted
  -> Either ErrorString Reformatted
asReformatter formatter r = (r <>) <$> formatter (reformattedSource r)
