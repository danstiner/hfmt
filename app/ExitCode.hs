module ExitCode
  ( exitCode
  , helpDoc
  , operationalFailureCode
  ) where

import Types

import System.Exit
import Text.PrettyPrint.ANSI.Leijen

-- | Decide which exit code to use
--
-- Assume if we are printing diffs that we *are* being run in a CI and so
-- should exit with failure if any format differences were found. Otherwise
-- assume we are being run in a context where non-zero exit indicates
-- failure of the tool to operate properly.
exitCode :: Action -> Bool -> ExitCode
exitCode action formattedCodeDiffers =
  if formattedCodeDiffers && failOnDifferences
    then ExitFailure formattedCodeDiffersFailureCode
    else ExitSuccess
  where
    failOnDifferences = action == PrintDiffs

helpDoc :: Doc
helpDoc =
  vsep
    [ text "Exit Codes:"
    , text "  0 = No error"
    , text "  1 = Encountered I/O or other operational error"
    , text "  2 = Failed to parse a source code file"
    , text "  3 = Source code was parsed but cannot be formatted properly"
    , text
        "  4 = Formatted code differs from existing source (--print-diffs only)"
    ]

-- | Encountered I/O or other operational error
operationalFailureCode :: Int
operationalFailureCode = 1

-- | Formatted code differs from existing source code
formattedCodeDiffersFailureCode :: Int
formattedCodeDiffersFailureCode = 4
