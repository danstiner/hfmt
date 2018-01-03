module Types
  ( Action(..)
  , FormatResult
  , FormatError(..)
  , Formatted(..)
  , HaskellSource(..)
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

data Formatted =
  Formatted SourceFile
            HaskellSource
            Reformatted
