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
            +
            (binary-op + 8 34)
            (+ 8 34)
          |]
          `shouldReturn` Right
            [ ValNil,
              ValNil,
              ValLambda (Stack []) ["a", "b"] $ ExprSymList [ExprSymbol "do", ExprSymList [ExprSymbol "+", ExprSymbol "a", ExprSymbol "b"]],
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
            [ ValNil,
              ValQuoted
                ( ExprSymList
                    [ ExprSymbol "hello",
                      ExprValue (ValNumber 23),
                      ExprSymbol "world",
                      ExprValue (ValNumber 20)
                    ]
                )
            ]

      context "when quote contains spliced unquote" $ do
        it "evaluates spliced unquote" $ do
          evalExpr
            [i|
              (set foobar '(1 2 3))
              (set value 200)
              '(hello ,@foobar world ,foobar)
              '(hello ,@value world ,value)
            |]
            `shouldReturn` Right
              [ ValNil,
                ValNil,
                ValQuoted
                  ( ExprSymList
                      [ ExprSymbol "hello",
                        ExprLiteral (LitNumber 1),
                        ExprLiteral (LitNumber 2),
                        ExprLiteral (LitNumber 3),
                        ExprSymbol "world",
                        ExprValue $
                          ValQuoted
                            (ExprSymList [ExprLiteral (LitNumber 1), ExprLiteral (LitNumber 2), ExprLiteral (LitNumber 3)])
                      ]
                  ),
                ValQuoted
                  (ExprSymList [ExprSymbol "hello", ExprValue (ValNumber 200), ExprSymbol "world", ExprValue (ValNumber 200)])
              ]

    context "list operations" $ do
      context "with car" $ do
        it "evaluates" $ do
          evalExpr
            [i|
              (car '(42 24 899))
              (car '(,(+ 20 5)))
              (car '())
            |]
            `shouldReturn` Right
              [ ValNumber 42,
                ValNumber 25,
                ValNil
              ]
      context "with cdr" $ do
        it "evaluates" $ do
          evalExpr
            [i|
              (cdr '(42 24 899))
              (cdr '(,(+ 20 5)))
              (cdr '())
            |]
            `shouldReturn` Right
              [ ValQuoted (ExprSymList [ExprValue (ValNumber 24), ExprValue (ValNumber 899)]),
                ValNil,
                ValNil
              ]
      context "with cons" $ do
        it "evaluates" $ do
          evalExpr
            [i|
              (cons 5 (cons 2 #nil))
              (cons 2 #nil)
              (cons (cons 5 #nil) (cons 2 #nil))
            |]
            `shouldReturn` Right
              [ ValQuoted (ExprSymList [ExprValue $ ValNumber 5.0, ExprValue (ValNumber 2.0)]),
                ValQuoted (ExprSymList [ExprValue (ValNumber 2.0)]),
                ValQuoted
                  ( ExprSymList
                      [ ExprValue $ ValQuoted (ExprSymList [ExprValue $ ValNumber 5.0]),
                        ExprValue (ValNumber 2.0)
                      ]
                  )
              ]

    context "define scopes" $ do
      it "lambda uses scope where it was defined" $ do
        evalExpr
          [i|
            (define (bar foobar) (foobar 5))
            (define (foo foobar) (bar (-> [x] (+ foobar x))))
            (foo 200)
          |]
          `shouldReturn` Right [ValNil, ValNil, ValNumber 205]

    context "var args" $ do
      it "allows accepting a variable number of arguments" $ do
        evalExpr
          [i|
            (define (foo a b c ... rest)
              '(,a ,b ,c ,rest))
            (foo 1 2 3 4 5 6 7)
          |]
          `shouldReturn` Right
            [ ValNil,
              ValQuoted $
                ExprSymList
                  [ ExprValue (ValNumber 1),
                    ExprValue (ValNumber 2),
                    ExprValue (ValNumber 3),
                    ExprValue $
                      ValQuoted $
                        ExprSymList
                          [ ExprValue (ValNumber 4),
                            ExprValue (ValNumber 5),
                            ExprValue (ValNumber 6),
                            ExprValue (ValNumber 7)
                          ]
                  ]
            ]

    context "factorial example" $ do
      it "evaluates successfully" $ do
        evalExpr
          [i|
            (define (factorial n)
              (if (lte? n 1)
                  1
                  (* n (factorial (- n 1)))))

            (factorial 10)
          |]
          `shouldReturn` Right [ValNil, ValNumber 3628800]
