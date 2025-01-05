module Main where

import qualified Aether.Runtime as Runtime
import qualified Aether.Syntax.Parser as Parser
import Data.String.Interpolate.IsString
import Text.Megaparsec (errorBundlePretty)

main :: IO ()
main = do
  let evalExpr code = do
        let results = Parser.parseAll "input" code
        case results of
          Right exprs -> Runtime.runInterpreter exprs
          Left e -> error $ errorBundlePretty e

  let code =
        [i|
    (define (fibo n)
      (if (lte? n 1)
          n
          (+ (fibo (- n 1)) (fibo (- n 2)))
      )
    )

    (fibo 25)
  |]
  evalExpr code >>= print
