module TestUtils where

import Aether.Syntax.Parser (AetherParser, Parsable, ParseError, parse)
import Data.Either (isLeft)
import Data.Text (Text)
import Test.Hspec
import Text.Megaparsec (ParseErrorBundle (ParseErrorBundle), runParser)

shouldParse :: (HasCallStack, Parsable v, Show v, Eq v) => Text -> v -> Expectation
shouldParse input val = parseParsable input `shouldBe` Right val

parseParsable :: (Parsable v, Show v, Eq v) => Text -> Either ParseError v
parseParsable = runParser parse "test-input"
