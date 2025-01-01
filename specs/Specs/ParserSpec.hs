module Specs.ParserSpec where

import Aether.Syntax.Parser
import Aether.Types
import Data.Either (isLeft)
import Test.Hspec
import TestUtils

test :: SpecWith ()
test = do
  describe "parse Expr" $ do
    context "when input is boolean literal" $ do
      it "parses boolean successfully" $ do
        "#t" `shouldParse` ExprLiteral (LitBool True)
        "#f" `shouldParse` ExprLiteral (LitBool False)
        (parseParsable "#a" :: ParserResult Expr) `shouldSatisfy` isLeft

    context "when input is nil literal" $ do
      it "parses boolean successfully" $ do
        "#nil" `shouldParse` ExprLiteral LitNil

    context "when input is symbolic expression" $ do
      it "parses expr successfully" $ do
        "()" `shouldParse` ExprSymList []
        "(#nil #t #f)"
          `shouldParse` ExprSymList [ExprLiteral LitNil, ExprLiteral $ LitBool True, ExprLiteral $ LitBool False]

    context "when input is nested symbolic expression" $ do
      it "parses expr successfully" $ do
        "(#nil (#t (#nil #f) #f))"
          `shouldParse` ExprSymList
            [ ExprLiteral LitNil,
              ExprSymList
                [ ExprLiteral $ LitBool True,
                  ExprSymList [ExprLiteral LitNil, ExprLiteral $ LitBool False],
                  ExprLiteral $ LitBool False
                ]
            ]
