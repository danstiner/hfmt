module Types (
    Action(..),
    InputFile(..),
    InputFileWithSource(..),
    ErrorString,
    ReformatResult(..),
    HaskellSourceFilePath,
    HaskellSource(..),
    ) where

import Language.Haskell.Format.Definitions
import Language.Haskell.Source.Enumerator  (HaskellSourceFilePath)

data Action = PrintDiffs
            | PrintSources
            | PrintFilePaths
            | WriteSources

data InputFile = InputFilePath HaskellSourceFilePath
               | InputFromStdIn

instance Show InputFile where
  show (InputFilePath path) = path
  show InputFromStdIn = "-"

data InputFileWithSource = InputFileWithSource InputFile HaskellSource

type ErrorString = String

data ReformatResult = InvalidReformat InputFile ErrorString
                    | Reformat InputFile HaskellSource Reformatted
