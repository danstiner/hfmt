module Types
  ( Action(..)
  , FormatResult
  , FormatError(..)
  , Formatted(..)
  , HaskellSource(..)
  , RunResult(..)
  , SourceFile(..)
  , SourceFileWithContents(..)
  ) where

import Language.Haskell.Format

data Action
  = PrintDiffs
  | PrintSources
  | PrintFilePaths
  | WriteSources
  deriving (Eq)

data SourceFile
  = SourceFilePath FilePath
  | StdinSource

instance Show SourceFile where
  show (SourceFilePath path) = path
  show StdinSource           = "-"

data SourceFileWithContents =
  SourceFileWithContents SourceFile
                         HaskellSource

type FormatResult = Either FormatError Formatted

data FormatError =
  FormatError SourceFile
              String

instance Show FormatError where
  show (FormatError input errorString) =
    "Error reformatting " ++ show input ++ ": " ++ errorString

data Formatted =
  Formatted SourceFile
            HaskellSource
            Reformatted

data RunResult
  = OperationalFailure
  | SourceParseFailure
  | HadDifferences
  | NoDifferences

instance Monoid RunResult where
  mempty = NoDifferences
  x `mappend` NoDifferences = x
  NoDifferences `mappend` x = x
  OperationalFailure `mappend` _ = OperationalFailure
  SourceParseFailure `mappend` _ = SourceParseFailure
  HadDifferences `mappend` _ = HadDifferences
