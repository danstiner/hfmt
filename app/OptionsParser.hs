module OptionsParser (Options, optAction, optPaths, parser) where

import Types

import Control.Applicative
import Data.List
import Data.Monoid
import Options.Applicative.Builder
import Options.Applicative.Common
import Options.Applicative.Extra
import Text.PrettyPrint.ANSI.Leijen hiding (empty, (<$>), (<>))

data Options =
       Options
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
optionParser = Options <$> switch
                             (long "print-diffs" <>
                              short 'd' <>
                              help "If a file's formatting is different, print a diff.")
                       <*> switch
                             (long "print-sources" <>
                              short 's' <>
                              help "If a file's formatting is different, print its source.")
                       <*> switch
                             (long "print-paths" <>
                              short 'l' <>
                              help "If a file's formatting is different, print its path.")
                       <*> switch
                             (long "write-sources" <>
                              short 'w' <>
                              help "If a file's formatting is different, overwrite it.")
                       <*> many pathOption
  where
    pathOption = strArgument (metavar "PATH" <> pathOptionHelp)
    pathOptionHelp = helpDoc $ Just $
      paragraph "Explicit paths to process." <> hardline
      <> unorderdedList " - "
           [ paragraph "A single '-' will process standard input."
           , paragraph "Files will be processed directly."
           , paragraph "Directories will be recursively searched for source files to process."
           , paragraph
               ".cabal files will be parsed and all specified source directories and files processed."
           , paragraph
               "If no paths are given, the current directory will be searched for .cabal files to process, if none are found the currend directory will be recursively searched for source files to process."
           ]

paragraph :: String -> Doc
paragraph = mconcat . intersperse softline . map text . words

unorderdedList :: String -> [Doc] -> Doc
unorderdedList prefix = encloseSep (text prefix) mempty (text prefix) . map (hang 0)

optionParserInfo :: ParserInfo Options
optionParserInfo = info (helper <*> optionParser)
                     (fullDesc
                      <> header "hfmt - formats Haskell programs"
                      <> progDesc
                           "Operates on Haskell source files, reformatting them by applying suggestions from hlint, hindent, and stylish-haskell. Inspired by the gofmt utility.")

parser :: ParserInfo Options
parser = optionParserInfo
