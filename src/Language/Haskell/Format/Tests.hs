module Language.Haskell.Format.Tests (hunit, hunitGlob) where

import Control.Monad
import Language.Haskell.Format as Format
import System.FilePath.Glob    (glob)
import Test.HUnit

type FilePathGlob = String

hunit :: [FilePath] -> Test
hunit = TestList . map hunitTestcase

hunitGlob :: FilePathGlob -> IO Test
hunitGlob pattern = fmap (TestList . map hunitTestcase) (glob pattern)

hunitTestcase :: FilePath -> Test
hunitTestcase path = TestLabel path . TestCase $ do
  settings <- Format.autoSettings
  result <- Format.checkPath settings path
  case result of
    Left err -> assertFailure ("Parse error in: " ++ path ++ "\n" ++ err)
    Right (ideas, formatted) -> if not (null ideas)
      then assertFailure (foldr ((++) . show) "" ideas)
      else assertOnFormatting formatted
  where
    assertOnFormatting (FormatResult before after) =
      when (before /= after) $ assertFailure "Formatting failure"
