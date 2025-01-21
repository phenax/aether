module TestUtils where

import Aether.Syntax.Parser (Parsable, ParseError, parse)
import Aether.Types
import Data.Text (Text)
import Test.Hspec
import Text.Megaparsec (MonadParsec (eof), SourcePos (SourcePos), errorBundlePretty, mkPos, runParser)

shouldParse :: (HasCallStack, Parsable v, Show v, Eq v) => Text -> v -> Expectation
shouldParse input val = case parseParsable input of
  Right res -> res `shouldBe` val
  Left e ->
    expectationFailure $
      "expected: " ++ show val ++ "\nbut parsing failed with error:\n" ++ errorBundlePretty e

parseParsable :: (Parsable v, Show v, Eq v) => Text -> Either ParseError v
parseParsable = runParser (parse <* eof) "test-input"

testPos :: Int -> Int -> SourcePos
testPos r c = SourcePos "test-input" (mkPos r) (mkPos c)

testSpan :: (Int, Int) -> (Int, Int) -> SourceSpan
testSpan (r1, c1) (r2, c2) = SourceSpan (testPos r1 c1) (testPos r2 c2)

-- TODO: replace some tests with real span
dummySpan :: SourceSpan
dummySpan = NullSpan
