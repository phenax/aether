module TestUtils where

import Aether.Syntax.Parser (Parsable, ParseError, Parser, parse)
import Data.Either (isLeft)
import Data.Text (Text)
import Test.Hspec
import Text.Megaparsec (MonadParsec (eof), ParseErrorBundle (ParseErrorBundle), errorBundlePretty, runParser)

shouldParse :: (HasCallStack, Parsable v, Show v, Eq v) => Text -> v -> Expectation
shouldParse input val = case parseParsable input of
  Right res -> res `shouldBe` val
  Left e ->
    expectationFailure $
      "expected: " ++ show val ++ "\nbut parsing failed with error:\n" ++ errorBundlePretty e

parseParsable :: (Parsable v, Show v, Eq v) => Text -> Either ParseError v
parseParsable = runParser (parse <* eof) "test-input"
