module Main where

import qualified Aether.Syntax.Parser as Parser
import Text.Megaparsec (errorBundlePretty)

main :: IO ()
main = do
  case Parser.parseAll "foobar.ae" "(#t #f nil (nil) ())" of
    Right x -> print x
    Left e -> putStrLn $ errorBundlePretty e
