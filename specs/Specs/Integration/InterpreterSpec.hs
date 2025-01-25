module Specs.Integration.InterpreterSpec where

import Aether.Runtime (runInterpreter)
import Aether.Syntax.Parser
import Aether.Types
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
            (fold + 0 '(1 2 3))
          |]
          `shouldReturn` Right [ValNil, ValNumber 27, ValNumber 6]

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
            (< 2 5) (< 5 2)
            (gt? 200 8) (gt? 8 200)
            (> 200 8) (> 8 200)
            (lte? 5 5) (lte? 5 2)
            (<= 5 5) (<= 5 2)
            (gte? 8 8) (gte? 8 200)
            (>= 8 8) (>= 8 200)
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

    describe "defmacro" $ do
      it "evaluates quoted list returned from macros" $ do
        evalExpr
          [i|
            (defmacro (foobar x) '(+ ,(car x) 5))
            (foobar '(1 2 3))
          |]
          `shouldReturn` Right [ValNil, ValNumber 6]

      context "when uses a spliced result" $ do
        it "evaluates macros" $ do
          evalExpr
            [i|
              (defmacro (foobar ls) '(list ,@(cons 0 ls)))
              (foobar (1 2 3))
            |]
            `shouldReturn` Right
              [ ValNil,
                ValQuoted $
                  ExprSymList
                    dummySpan
                    [ ExprValue $ ValNumber 0,
                      ExprValue $ ValNumber 1,
                      ExprValue $ ValNumber 2,
                      ExprValue $ ValNumber 3
                    ]
              ]

      context "when uses a spliced result" $ do
        it "evaluates macros" $ do
          evalExpr
            [i|
              (defmacro (foobar ls) '(list ,@(map (-> [x] '(list ,@x)) ls)))
              (foobar ((1) (2) (3)))
            |]
            `shouldReturn` Right
              [ ValNil,
                ValQuoted $
                  ExprSymList
                    dummySpan
                    [ ExprValue $ ValQuoted $ ExprSymList dummySpan [ExprValue $ ValNumber 1],
                      ExprValue $ ValQuoted $ ExprSymList dummySpan [ExprValue $ ValNumber 2],
                      ExprValue $ ValQuoted $ ExprSymList dummySpan [ExprValue $ ValNumber 3]
                    ]
              ]

    describe "quasiquotes : unquote and splice" $ do
      it "evaluates unquotes" $ do
        evalExpr
          [i|
            (define foobar 20)
            '(hello ,(+ 20 3) world ,foobar)
          |]
          `shouldReturn` Right
            [ ValNil,
              ValQuoted
                ( ExprSymList
                    dummySpan
                    [ ExprSymbol dummySpan "hello",
                      ExprValue (ValNumber 23),
                      ExprSymbol dummySpan "world",
                      ExprValue (ValNumber 20)
                    ]
                )
            ]

      context "when quote contains spliced unquote" $ do
        it "evaluates spliced unquote" $ do
          evalExpr
            [i|
              (define foobar '(1 2 3))
              (define value 200)
              '(hello ,@foobar world ,foobar)
              '(hello ,@value world ,value)
            |]
            `shouldReturn` Right
              [ ValNil,
                ValNil,
                ValQuoted
                  ( ExprSymList
                      dummySpan
                      [ ExprSymbol dummySpan "hello",
                        ExprLiteral dummySpan (LitNumber 1),
                        ExprLiteral dummySpan (LitNumber 2),
                        ExprLiteral dummySpan (LitNumber 3),
                        ExprSymbol dummySpan "world",
                        ExprValue $
                          ValQuoted
                            (ExprSymList dummySpan [ExprLiteral dummySpan (LitNumber 1), ExprLiteral dummySpan (LitNumber 2), ExprLiteral dummySpan (LitNumber 3)])
                      ]
                  ),
                ValQuoted
                  (ExprSymList dummySpan [ExprSymbol dummySpan "hello", ExprValue (ValNumber 200), ExprSymbol dummySpan "world", ExprValue (ValNumber 200)])
              ]

      context "with nested unquotes" $ do
        it "nested unquotes" $ do
          evalExpr
            [i|
              (define foobar 20)
              (define list '(1 2 3))
              '(hello (+ ,foobar ,@list) world)
            |]
            `shouldReturn` Right
              [ ValNil,
                ValNil,
                ValQuoted
                  ( ExprSymList
                      dummySpan
                      [ ExprSymbol dummySpan "hello",
                        ExprSymList
                          dummySpan
                          [ ExprSymbol dummySpan "+",
                            ExprValue (ValNumber 20),
                            ExprLiteral dummySpan (LitNumber 1),
                            ExprLiteral dummySpan (LitNumber 2),
                            ExprLiteral dummySpan (LitNumber 3)
                          ],
                        ExprSymbol dummySpan "world"
                      ]
                  )
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
              [ ValQuoted (ExprSymList dummySpan [ExprValue (ValNumber 24), ExprValue (ValNumber 899)]),
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
              [ ValQuoted (ExprSymList dummySpan [ExprValue $ ValNumber 5.0, ExprValue (ValNumber 2.0)]),
                ValQuoted (ExprSymList dummySpan [ExprValue (ValNumber 2.0)]),
                ValQuoted
                  ( ExprSymList
                      dummySpan
                      [ ExprValue $ ValQuoted (ExprSymList dummySpan [ExprValue $ ValNumber 5.0]),
                        ExprValue (ValNumber 2.0)
                      ]
                  )
              ]

    describe "example/factorial" $ do
      it "evaluates successfully" $ do
        evalExpr
          [i|
            (define (factorial n)
              (if (<= n 1)
                  1
                  (* n (factorial (- n 1)))))

            (factorial 10)
          |]
          `shouldReturn` Right [ValNil, ValNumber 3628800]
