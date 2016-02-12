module Language.Haskell.Format.HLint (autoSettings, formatter, suggester) where

import           Language.Haskell.Format.Definitions
import           Language.Haskell.Format.Internal

import           Control.Applicative
import           Language.Haskell.Exts.Annotated     as Hse
import qualified Language.Haskell.HLint3             as HLint3

formatter = undefined

suggester :: (ParseMode, [HLint3.Classify], HLint3.Hint) -> Formatter
suggester = mkSuggester . hlint

hlint :: (ParseMode, [HLint3.Classify], HLint3.Hint) -> HaskellSource -> Either String [Suggestion]
hlint (parseMode, classifications, hint) (HaskellSource source) =
  getSuggestions <$> parseResultAsEither (parse source)
  where
    parse = Hse.parseFileContentsWithComments parseMode
    getSuggestions moduleSource = map ideaToSuggestion $ HLint3.applyHints classifications hint
                                                           [moduleSource]

autoSettings :: IO (ParseMode, [HLint3.Classify], HLint3.Hint)
autoSettings = do
  (fixities, classify, hints) <- HLint3.findSettings (HLint3.readSettingsFile Nothing) Nothing
  return
    (HLint3.hseFlags (HLint3.parseFlagsAddFixities fixities HLint3.defaultParseFlags), classify, HLint3.resolveHints
                                                                                                   hints)

parseResultAsEither :: ParseResult a -> Either String a
parseResultAsEither (ParseOk a) = Right a
parseResultAsEither (ParseFailed loc error) =
  Left ("Parse failed at " ++ location ++ ": " ++ error)
  where
    location = filename ++ " " ++ linecol
    filename = "[" ++ srcFilename loc ++ "]"
    linecol = "(" ++ show (srcLine loc) ++ ":" ++ show (srcColumn loc) ++ ")"

ideaToSuggestion :: HLint3.Idea -> Suggestion
ideaToSuggestion = Suggestion . show
