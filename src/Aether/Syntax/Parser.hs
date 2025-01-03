module Aether.Syntax.Parser where

import Aether.Types
import Control.Applicative ((<|>))
import Control.Monad (void)
import Control.Monad.Identity (Identity)
import Data.Char (isAlpha, isAlphaNum)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Void (Void)
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as P

type ParseError = P.ParseErrorBundle Text Void

type ParserResult p = (Parsable p) => Either ParseError p

type Parser = P.ParsecT Void Text Identity

class Parsable a where
  parse :: Parser a

spaceConsumer :: Parser ()
spaceConsumer = void $ P.many (P.newline <|> P.spaceChar <|> P.tab)

instance Parsable Literal where
  parse = boolP <|> nilP <|> stringP <|> numberP
    where
      nilP = LitNil <$ P.string "#nil"

      boolP = LitBool . (== "#T") <$> (P.string "#T" <|> P.string "#F")

      numberP = LitNumber <$> numberUnsigned
        where
          numberUnsigned = do
            -- TODO: Rethink signs
            -- sign <- maybe "" (: "") <$> P.optional (P.char '-')
            let sign = ""
            decimals <- P.some P.digitChar
            P.optional $ P.char '.'
            floating <- fromMaybe "0" <$> P.optional (P.some P.digitChar)
            pure . read $ sign ++ decimals ++ "." ++ floating

      stringP =
        LitString <$> do
          P.between (P.char '"') (P.char '"') $ P.many (P.satisfy (/= '"'))

instance Parsable Expr where
  parse = spaceConsumer >> (quotedP <|> literalP <|> symExprP <|> symbolP) <* spaceConsumer
    where
      literalP = ExprLiteral <$> parse

      symExprP = ExprSymList <$> (parens contents <|> brackets contents <|> angles contents <|> braces contents)
        where
          contents = P.sepBy parse spaceConsumer
          parens = P.between (P.char '(') (P.char ')')
          brackets = P.between (P.char '[') (P.char ']')
          angles = P.between (P.char '<') (P.char '>')
          braces = P.between (P.char '{') (P.char '}')

      quotedP = ExprQuoted <$> (P.char '\'' >> parse)

      symbolP = ExprSymbol <$> ((:) <$> identStartChar <*> P.many identChar)
        where
          identStartChar = P.satisfy $ \c -> isAlpha c || c `elem` (":-_+=|?!@$%^&*/\\~." :: String)
          identChar = P.satisfy $ \c -> isAlphaNum c || c `elem` ("'#:-_+=|?!@$%^&*/\\~." :: String)

instance Parsable [Expr] where
  parse = P.manyTill parse P.eof

parseAll :: String -> Text -> Either ParseError [Expr]
parseAll = P.runParser parse
