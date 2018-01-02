module Language.Haskell.Format.HLint
  ( autoSettings
  , suggester
  ) where

import           Language.Haskell.Format.Internal
import           Language.Haskell.Format.Types

import           Language.Haskell.HLint3          (Classify, Hint,
                                                   ParseError (..), ParseFlags,
                                                   applyHints, parseModuleEx)
import qualified Language.Haskell.HLint3          as HLint3
import           System.IO.Unsafe                 (unsafePerformIO)

suggester :: (ParseFlags, [Classify], Hint) -> Formatter
suggester = mkSuggester . hlint

hlint ::
     (ParseFlags, [Classify], Hint)
  -> HaskellSource
  -> Either String [Suggestion]
hlint (flags, classify, hint) (HaskellSource filepath source) =
  case unsafePerformIO (parseModuleEx flags filepath (Just source)) of
    Right m         -> Right . map ideaToSuggestion . ideas $ m
    Left parseError -> Left . parseErrorMessage $ parseError
  where
    ideas m = applyHints classify hint [m]

autoSettings :: IO (ParseFlags, [Classify], Hint)
autoSettings = HLint3.autoSettings

ideaToSuggestion :: HLint3.Idea -> Suggestion
ideaToSuggestion = Suggestion . show
