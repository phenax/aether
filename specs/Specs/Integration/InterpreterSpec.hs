module Specs.Integration.InterpreterSpec where

import Aether.Runtime.Interpreter (interpretExpression, runExprEvaluatorWithCallStack, runExprInterpreter, runInterpreter)
import Aether.Syntax.Parser
import Aether.Types
import Data.Either (isLeft)
import Data.String.Interpolate.IsString
import Test.Hspec
import TestUtils
import Text.Megaparsec (errorBundlePretty)

test :: SpecWith ()
test = do
  let evalExpr code = do
        let results = parseAll "input" code
        case results of
          Right exprs -> runInterpreter exprs
          Left e -> error $ errorBundlePretty e

  describe "parse and interpret" $ do
    context "addition" $ do
      it "evaluates successfully" $ do
        evalExpr
          [i|
            (define (foobar a b) (+ a b))
            (foobar 20 7)
          |]
          `shouldReturn` Right [ValNil, ValNumber 27.0]
    context "with overridden symbol for addition" $ do
      it "evaluates successfully" $ do
        evalExpr
          [i|
            (define (binary-op op a b) (op a b))
            (set + (-> [a b] (+ a b)))
            (binary-op + 8 34)
            (+ 8 34)
          |]
          `shouldReturn` Right
            [ ValNil,
              ValLambda ["a", "b"] $ ExprSymList [ExprSymbol "do", ExprSymList [ExprSymbol "+", ExprSymbol "a", ExprSymbol "b"]],
              ValNumber 42.0,
              ValNumber 42.0
            ]
