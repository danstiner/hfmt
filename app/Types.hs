module Types
  ( Action(..)
  , FormatResult
  , FormatError(..)
  , Formatted(..)
  , HaskellSourceFilePath
  , HaskellSource(..)
  , SourceFile(..)
  , SourceFileWithContents(..)
  ) where

import Language.Haskell.Format
import Language.Haskell.Source.Enumerator (HaskellSourceFilePath)

data Action
  = PrintDiffs
  | PrintSources
  | PrintFilePaths
  | WriteSources
  deriving (Eq)

data SourceFile
  = SourceFilePath HaskellSourceFilePath
  | SourceFromStdIn

instance Show SourceFile where
  show (SourceFilePath path) = path
  show SourceFromStdIn       = "-"

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
