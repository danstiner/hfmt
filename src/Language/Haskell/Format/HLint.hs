module Language.Haskell.Format.HLint (autoSettings, formatter, suggester) where

import Language.Haskell.Format.Definitions
import Language.Haskell.Format.Internal

import Control.Applicative
import Data.Maybe
import Language.Haskell.Exts.Annotated     as Hse
import Language.Haskell.HLint3             hiding (autoSettings)

type Settings = (ParseMode, [Classify], Hint)

formatter = undefined

suggester :: (ParseMode, [Classify], Hint) -> Formatter
suggester = mkSuggester . hlint

hlint :: (ParseMode, [Classify], Hint) -> HaskellSource -> Either String [Suggestion]
hlint (parseMode, classifications, hint) (HaskellSource source) =
  getSuggestions <$> parseResultAsEither (parse source)
  where
    parse = Hse.parseFileContentsWithComments parseMode
    getSuggestions moduleSource = map ideaToSuggestion $ applyHints classifications hint
                                                           [moduleSource]

autoSettings :: IO (ParseMode, [Classify], Hint)
autoSettings = do
  (fixities, classify, hints) <- findSettings (readSettingsFile Nothing) Nothing
  return (hseFlags (parseFlagsAddFixities fixities defaultParseFlags), classify, resolveHints hints)

parseResultAsEither :: ParseResult a -> Either String a
parseResultAsEither (ParseOk a) = Right a
parseResultAsEither (ParseFailed loc error) =
  Left ("Parse failed at " ++ location ++ ": " ++ error)
  where
    location = filename ++ " " ++ linecol
    filename = "[" ++ srcFilename loc ++ "]"
    linecol = "(" ++ show (srcLine loc) ++ ":" ++ show (srcColumn loc) ++ ")"

ideaToSuggestion :: Idea -> Suggestion
ideaToSuggestion = Suggestion . show
