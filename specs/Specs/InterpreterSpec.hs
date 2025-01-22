module Specs.InterpreterSpec where

import Aether.Runtime (runExprInterpreter, runInterpreter)
import Aether.Types
import Test.Hspec
import TestUtils

test :: SpecWith ()
test = do
  let evalExpr = runExprInterpreter
  let evalExprs = runInterpreter

  describe "interpreter" $ do
    context "when input is a literal" $ do
      it "evaluates successfully" $ do
        evalExpr (ExprLiteral dummySpan $ LitBool True) `shouldReturn` Right (ValBool True)
        evalExpr (ExprLiteral dummySpan $ LitBool False) `shouldReturn` Right (ValBool False)
        evalExpr (ExprLiteral dummySpan $ LitNumber 42.14159) `shouldReturn` Right (ValNumber 42.14159)
        evalExpr (ExprLiteral dummySpan $ LitString "hellow") `shouldReturn` Right (ValString "hellow")

    context "with different forms of nil" $ do
      it "evaluates to nil" $ do
        evalExpr (ExprLiteral dummySpan LitNil) `shouldReturn` Right ValNil
        evalExpr (ExprSymList dummySpan []) `shouldReturn` Right ValNil
        evalExpr (ExprQuoted dummySpan (ExprSymList dummySpan [])) `shouldReturn` Right ValNil

    context "when setting/getting a symbol" $ do
      context "when a symbol was defined" $ do
        it "gets value of symbol" $ do
          let exprs =
                [ ExprSymList dummySpan [ExprSymbol dummySpan "define", ExprSymbol dummySpan "foobar", ExprLiteral dummySpan $ LitNumber 923],
                  ExprSymbol dummySpan "foobar"
                ]
          evalExprs exprs `shouldReturn` Right [ValNil, ValNumber 923]

      context "when symbol does not exist" $ do
        it "fails with error" $ do
          evalExpr (ExprSymbol dummySpan "invalidsymbol") `shouldReturn` Left (NameNotFound "invalidsymbol")

    context "when evaluating a do call" $ do
      it "returns with the last expression in body" $ do
        evalExpr (ExprSymList dummySpan [ExprSymbol dummySpan "do", ExprLiteral dummySpan $ LitNumber 1, ExprLiteral dummySpan $ LitNumber 2, ExprLiteral dummySpan $ LitNumber 3])
          `shouldReturn` Right (ValNumber 3)

      context "when there are no expressions in body" $ do
        it "returns nil" $ do
          evalExpr (ExprSymList dummySpan [ExprSymbol dummySpan "do"]) `shouldReturn` Right ValNil

    context "when calling a lambda" $ do
      it "returns with the last expression in body" $ do
        let exprs =
              [ ExprSymList
                  dummySpan
                  [ ExprSymbol dummySpan "define",
                    ExprSymList dummySpan [ExprSymbol dummySpan "foobar", ExprSymbol dummySpan "a", ExprSymbol dummySpan "b"],
                    ExprSymbol dummySpan "b",
                    ExprSymbol dummySpan "a"
                  ],
                ExprSymList dummySpan [ExprSymbol dummySpan "foobar", ExprLiteral dummySpan $ LitNumber 42, ExprLiteral dummySpan $ LitNumber 99]
              ]
        evalExprs exprs `shouldReturn` Right [ValNil, ValNumber 42.0]

      context "when trying to use value defined inside a lambda" $ do
        it "returns original value" $ do
          let exprs =
                [ ExprSymList dummySpan [ExprSymbol dummySpan "define", ExprSymbol dummySpan "foobar", ExprLiteral dummySpan $ LitNumber 42],
                  ExprSymList
                    dummySpan
                    [ ExprSymbol dummySpan "define",
                      ExprSymList dummySpan [ExprSymbol dummySpan "myfunc"],
                      ExprSymList dummySpan [ExprSymbol dummySpan "define", ExprSymbol dummySpan "foobar", ExprLiteral dummySpan $ LitNumber 69]
                    ],
                  ExprSymList dummySpan [ExprSymbol dummySpan "myfunc"],
                  ExprSymbol dummySpan "foobar"
                ]
          evalExprs exprs `shouldReturn` Right [ValNil, ValNil, ValNil, ValNumber 42.0]

      context "when getting symbol of lambda" $ do
        it "returns with the last expression in body" $ do
          let exprs =
                [ ExprSymList
                    dummySpan
                    [ ExprSymbol dummySpan "define",
                      ExprSymList dummySpan [ExprSymbol dummySpan "foobar", ExprSymbol dummySpan "a"],
                      ExprSymbol dummySpan "a"
                    ],
                  ExprSymbol dummySpan "foobar"
                ]
          evalExprs exprs
            `shouldReturn` Right [ValNil, ValLambda (Stack []) dummySpan ["a"] $ ExprSymbol dummySpan "a"]

    context "when calling a boolean" $ do
      context "when value is true (#T)" $ do
        it "evaluates and returns first argument" $ do
          let exprs = ExprSymList dummySpan [ExprLiteral dummySpan $ LitBool True, ExprLiteral dummySpan $ LitNumber 42, ExprLiteral dummySpan $ LitNumber 99]
          evalExpr exprs `shouldReturn` Right (ValNumber 42)

      context "when value is true (#F)" $ do
        it "evaluates and returns first argument" $ do
          let exprs = ExprSymList dummySpan [ExprLiteral dummySpan $ LitBool False, ExprLiteral dummySpan $ LitNumber 42, ExprLiteral dummySpan $ LitNumber 99]
          evalExpr exprs `shouldReturn` Right (ValNumber 99)

      context "when there are no expressions in body" $ do
        it "returns nil" $ do
          let exprs =
                [ ExprSymList
                    dummySpan
                    [ ExprSymbol dummySpan "define",
                      ExprSymList dummySpan [ExprSymbol dummySpan "foobar", ExprSymbol dummySpan "a", ExprSymbol dummySpan "b"]
                    ],
                  ExprSymList dummySpan [ExprSymbol dummySpan "foobar", ExprLiteral dummySpan $ LitNumber 42, ExprLiteral dummySpan $ LitNumber 99]
                ]
          evalExprs exprs `shouldReturn` Right [ValNil, ValNil]

    context "when calling eval" $ do
      it "returns with the last expression in body" $ do
        let expr =
              ExprSymList
                dummySpan
                [ ExprSymbol dummySpan "eval",
                  ExprQuoted dummySpan $ ExprSymList dummySpan [ExprSymbol dummySpan "-", ExprLiteral dummySpan $ LitNumber 200, ExprLiteral dummySpan $ LitNumber 15]
                ]
        evalExpr expr `shouldReturn` Right (ValNumber 185)
