module Aether.Syntax.Parser where

import Aether.Types
import Control.Applicative ((<|>))
import Control.Monad.Identity (Identity)
import Data.Char (isAlpha, isAlphaNum)
import Data.List ((\\))
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Void (Void)
import Text.Megaparsec (getSourcePos)
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as P
import qualified Text.Megaparsec.Char.Lexer as L

type ParseError = P.ParseErrorBundle Text Void

type ParserResult p = (Parsable p) => Either ParseError p

type Parser = P.ParsecT Void Text Identity

class Parsable a where
  parse :: Parser a

spaceConsumer :: Parser ()
spaceConsumer = L.space P.space1 singleLineComment multiLineComment
  where
    singleLineComment = L.skipLineComment ";"
    multiLineComment = L.skipBlockComment "#|" "|#"

parseWithSpan :: Parser a -> Parser (SourceSpan, a)
parseWithSpan parser = do
  spaceConsumer
  (start, result, end) <-
    (,,) <$> getSourcePos <*> parser <*> getSourcePos
  spaceConsumer
  pure (SourceSpan start end, result)

instance Parsable Literal where
  parse = boolP <|> nilP <|> stringP <|> numberP
    where
      nilP = LitNil <$ P.string "#nil"

      boolP = LitBool . (== "#T") <$> (P.string "#T" <|> P.string "#F")

      numberP = LitNumber <$> numberUnsigned
        where
          numberUnsigned = do
            decimals <- P.some P.digitChar
            P.optional $ P.char '.'
            floating <- fromMaybe "0" <$> P.optional (P.some P.digitChar)
            pure . read $ decimals ++ "." ++ floating

      stringP =
        LitString <$> do
          P.between (P.char '"') (P.char '"') $ P.many (P.satisfy (/= '"'))

instance Parsable Expr where
  parse =
    spaceConsumer >> (quotedP <|> splicedP <|> unQuotedP <|> literalP <|> symExprP <|> symbolP) <* spaceConsumer
    where
      literalP = uncurry ExprLiteral <$> parseWithSpan parse

      symExprP = uncurry ExprSymList <$> (parens contents <|> brackets contents <|> braces contents)
        where
          contents :: Parser (SourceSpan, [Expr])
          contents = parseWithSpan $ P.sepBy parse spaceConsumer
          parens = P.between (P.char '(') (P.char ')')
          brackets = P.between (P.char '[') (P.char ']')
          braces = P.between (P.char '{') (P.char '}')

      quotedP = uncurry ExprQuoted <$> parseWithSpan (P.char '\'' >> parse)
      unQuotedP = uncurry ExprUnquoted <$> parseWithSpan (P.char ',' >> parse)
      splicedP = uncurry ExprSpliced <$> parseWithSpan (P.string ",@" >> parse)

      symbolP = uncurry ExprSymbol <$> parseWithSpan ((:) <$> identStartChar <*> P.many identChar)
        where
          validIdentSpecialChars = ":-_+=|?!@$%^&*/\\~.'#<>" :: [Char]
          invalidStartChars = "'#@" :: [Char]
          identStartChar = P.satisfy $ \c -> isAlpha c || c `elem` (validIdentSpecialChars \\ invalidStartChars)
          identChar = P.satisfy $ \c -> isAlphaNum c || c `elem` validIdentSpecialChars

instance Parsable [Expr] where
  parse = P.many parse

parseAll :: String -> Text -> Either ParseError [Expr]
parseAll = P.runParser (parse <* P.eof)
