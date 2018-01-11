module Options
  ( Options
  , optAction
  , optPaths
  , parser
  ) where

import qualified ExitCode
import           Types

import           Control.Applicative
import           Data.Monoid
import           Options.Applicative.Builder
import           Options.Applicative.Common
import           Options.Applicative.Extra
import           Text.PrettyPrint.ANSI.Leijen as Leijen hiding ((<$>), (<>))

data Options = Options
  { optPrintDiffs     :: Bool
  , optPrintSources   :: Bool
  , optPrintFilePaths :: Bool
  , optWriteSources   :: Bool
  , optPaths          :: [FilePath]
  }

optAction :: Options -> Action
optAction options
  | optPrintDiffs options = PrintDiffs
  | optPrintFilePaths options = PrintFilePaths
  | optPrintSources options = PrintSources
  | optWriteSources options = WriteSources
  | optPaths options == ["-"] = PrintSources
  | otherwise = PrintDiffs

optionParser :: Parser Options
optionParser =
  Options <$>
  switch
    (long "print-diffs" <> short 'd' <>
     help "If a file's formatting is different, print a diff.") <*>
  switch
    (long "print-sources" <> short 's' <> hidden <>
     help "If a file's formatting is different, print its source.") <*>
  switch
    (long "print-paths" <> short 'l' <> hidden <>
     help "If a file's formatting is different, print its path.") <*>
  switch
    (long "write-sources" <> short 'w' <> hidden <>
     help "If a file's formatting is different, overwrite it.") <*>
  many pathOption
  where
    pathOption = strArgument (metavar "FILE" <> pathOptionHelp)
    pathOptionHelp =
      helpDoc $
      Just $
      text "Explicit paths to process." <> line <>
      unorderdedList
        " - "
        [ text "A single '-' will process standard input."
        , text "Files will be processed directly."
        , text
            "Directories will be recursively searched for source files to process."
        , text
            ".cabal files will be parsed and all specified source directories and files processed."
        , text
            "If no paths are given, the current directory will be searched for .cabal files to process, if none are found the current directory will be recursively searched for source files to process."
        ]

unorderdedList :: String -> [Doc] -> Doc
unorderdedList prefix = vsep . map ((text prefix <>) . align)

optionParserInfo :: ParserInfo Options
optionParserInfo =
  info
    (helper <*> optionParser)
    (fullDesc <> header "hfmt - Format Haskell programs" <>
     progDesc
       "Reformats Haskell source files by applying HLint, hindent, and stylish-haskell." <>
     footerDoc (Just ExitCode.helpDoc) <>
     failureCode ExitCode.operationalFailureCode)

parser :: ParserInfo Options
parser = optionParserInfo
