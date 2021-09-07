module Language.Haskell.Format.HLint
  ( autoSettings
  , suggester
  ) where

import Language.Haskell.Format.Internal
import Language.Haskell.Format.Types

import Language.Haskell.HLint           (Classify, Hint, Idea, ParseError (..),
                                         ParseFlags, applyHints, argsSettings,
                                         parseModuleEx)
import System.IO.Unsafe                 (unsafePerformIO)

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
autoSettings = argsSettings []

ideaToSuggestion :: Idea -> Suggestion
ideaToSuggestion = Suggestion . show
