module Specs.InterpreterSpec where

import Aether.Runtime.Interpreter (interpretExpression, runExprEvaluatorWithCallStack, runExprInterpreter, runInterpreter)
import Aether.Syntax.Parser
import Aether.Types
import Data.Either (isLeft)
import Data.String.Interpolate.IsString
import Test.Hspec
import TestUtils

test :: SpecWith ()
test = do
  let evalExpr = runExprInterpreter
  let evalExprs = runInterpreter

  fdescribe "interpreter" $ do
    context "when input is a literal" $ do
      it "evaluates successfully" $ do
        evalExpr (ExprLiteral $ LitBool True) `shouldReturn` Right (ValBool True)
        evalExpr (ExprLiteral $ LitBool False) `shouldReturn` Right (ValBool False)
        evalExpr (ExprLiteral $ LitNumber 42.14159) `shouldReturn` Right (ValNumber 42.14159)
        evalExpr (ExprLiteral $ LitString "hellow") `shouldReturn` Right (ValString "hellow")

    context "with different forms of nil" $ do
      it "evaluates to nil" $ do
        evalExpr (ExprLiteral LitNil) `shouldReturn` Right ValNil
        evalExpr (ExprSymList []) `shouldReturn` Right ValNil
        evalExpr (ExprQuoted (ExprSymList [])) `shouldReturn` Right ValNil

    context "when setting/getting a symbol" $ do
      context "when a symbol was set" $ do
        it "gets value of symbol" $ do
          let exprs =
                [ ExprSymList [ExprSymbol "set", ExprSymbol "foobar", ExprLiteral $ LitNumber 923],
                  ExprSymbol "foobar"
                ]
          evalExprs exprs `shouldReturn` Right [ValNumber 923, ValNumber 923]

      context "when symbol does not exist" $ do
        it "fails with error" $ do
          evalExpr (ExprSymbol "invalidsymbol") `shouldReturn` Left (NameNotFound "invalidsymbol")

    context "when evaluating a do call" $ do
      it "returns with the last expression in body" $ do
        evalExpr (ExprSymList [ExprSymbol "do", ExprLiteral $ LitNumber 1, ExprLiteral $ LitNumber 2, ExprLiteral $ LitNumber 3])
          `shouldReturn` Right (ValNumber 3)

      context "when there are no expressions in body" $ do
        it "returns nil" $ do
          evalExpr (ExprSymList [ExprSymbol "do"]) `shouldReturn` Right ValNil

    context "when calling a lambda" $ do
      it "returns with the last expression in body" $ do
        let exprs =
              [ ExprSymList
                  [ ExprSymbol "define",
                    ExprSymList [ExprSymbol "foobar", ExprSymbol "a", ExprSymbol "b"],
                    ExprSymbol "b",
                    ExprSymbol "a"
                  ],
                ExprSymList [ExprSymbol "foobar", ExprLiteral $ LitNumber 42, ExprLiteral $ LitNumber 99]
              ]
        evalExprs exprs `shouldReturn` Right [ValNil, ValNumber 42.0]

      context "when getting symbol of lambda" $ do
        it "returns with the last expression in body" $ do
          let exprs =
                [ ExprSymList
                    [ ExprSymbol "define",
                      ExprSymList [ExprSymbol "foobar", ExprSymbol "a"],
                      ExprSymbol "a"
                    ],
                  ExprSymbol "foobar"
                ]
          evalExprs exprs
            `shouldReturn` Right [ValNil, ValLambda ["a"] $ ExprSymList [ExprSymbol "do", ExprSymbol "a"]]

      context "when there are no expressions in body" $ do
        it "returns nil" $ do
          let exprs =
                [ ExprSymList
                    [ ExprSymbol "define",
                      ExprSymList [ExprSymbol "foobar", ExprSymbol "a", ExprSymbol "b"]
                    ],
                  ExprSymList [ExprSymbol "foobar", ExprLiteral $ LitNumber 42, ExprLiteral $ LitNumber 99]
                ]
          evalExprs exprs `shouldReturn` Right [ValNil, ValNil]
