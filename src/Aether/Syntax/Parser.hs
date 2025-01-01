module Aether.Syntax.Parser where

import Aether.Types
import Control.Applicative ((<|>))
import Control.Monad.Identity (Identity)
import Data.Text (Text)
import Data.Void (Void)
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as P

type ParseError = P.ParseErrorBundle Text Void

type ParserResult p = (Parsable p) => Either ParseError p

type AetherParser = P.ParsecT Void Text Identity

class Parsable a where
  parse :: AetherParser a

whitespaceP :: AetherParser String
whitespaceP = P.many (P.newline <|> P.spaceChar <|> P.tab)

instance Parsable Literal where
  parse = boolP <|> nilP
    where
      nilP = LitNil <$ P.string "#nil"
      boolP = LitBool . (== "#t") <$> (P.string "#t" <|> P.string "#f")

instance Parsable Expr where
  parse = literalP <|> sexP
    where
      literalP = ExprLiteral <$> parse
      sexP = ExprSymList <$> P.between (P.char '(') (P.char ')') (P.sepBy parse whitespaceP)

parseAll :: String -> Text -> Either ParseError [Expr]
parseAll = P.runParser (P.manyTill parse P.eof)
