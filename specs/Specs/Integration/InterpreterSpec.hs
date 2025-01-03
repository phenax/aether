module Specs.Integration.InterpreterSpec where

import Aether.Runtime (runInterpreter)
import Aether.Syntax.Parser
import Aether.Types
import Data.String.Interpolate.IsString
import Test.Hspec
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

    context "with math operators" $ do
      it "evaluates successfully" $ do
        evalExpr
          [i|
            (+ 2.5 5 8 2) (+ 2.5)
            (- 20 5 200) (- 20)
            (* 2.5 5 8 2) (* 2.5)
            (/ 300 3 8) (/ 42)
          |]
          `shouldReturn` Right
            [ ValNumber 17.5,
              ValNumber 2.5,
              ValNumber (-185),
              ValNumber (-20),
              ValNumber 200,
              ValNumber 2.5,
              ValNumber 12.5,
              ValNumber 42
            ]

    context "with comparison operators" $ do
      it "evaluates successfully" $ do
        evalExpr
          [i|
            (lt? 2 5) (lt? 5 2)
            (gt? 200 8) (gt? 8 200)
            (lte? 5 5) (lte? 5 2)
            (gte? 8 8) (gte? 8 200)
            (eq? 8 8 8 8 8) (eq? 8 8 5 8 8)
          |]
          `shouldReturn` Right
            [ ValBool True,
              ValBool False,
              ValBool True,
              ValBool False,
              ValBool True,
              ValBool False,
              ValBool True,
              ValBool False,
              ValBool True,
              ValBool False
            ]

    context "with boolean operators" $ do
      it "evaluates successfully" $ do
        evalExpr
          [i|
            (&& #T #T #F) (&& #T #T #T) (&&)
            (|| #F #F #T) (|| #F #F #F) (||)
            (not #T) (not #F)
          |]
          `shouldReturn` Right
            [ ValBool False,
              ValBool True,
              ValBool True,
              ValBool True,
              ValBool False,
              ValBool False,
              ValBool False,
              ValBool True
            ]

    context "quasiquotes" $ do
      it "evaluates unquotes" $ do
        evalExpr
          [i|
            (set foobar 20)
            '(hello ,(+ 20 3) world ,foobar)
          |]
          `shouldReturn` Right
            [ ValNumber 20,
              ValQuoted
                ( ExprSymList
                    [ ExprSymbol "hello",
                      ExprValue (ValNumber 23),
                      ExprSymbol "world",
                      ExprValue (ValNumber 20)
                    ]
                )
            ]
