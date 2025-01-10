module Specs.ParserSpec where

import Aether.Syntax.Parser
import Aether.Types
import Data.Either (isLeft)
import Data.String.Interpolate.IsString
import Test.Hspec
import TestUtils

test :: SpecWith ()
test = do
  describe "parse Expr" $ do
    context "when input is boolean literal" $ do
      it "parses boolean successfully" $ do
        "#T" `shouldParse` ExprLiteral (LitBool True)
        "#F" `shouldParse` ExprLiteral (LitBool False)

    context "when input is nil literal" $ do
      it "parses boolean successfully" $ do
        "#nil" `shouldParse` ExprLiteral LitNil

    context "when input is a symbol" $ do
      it "parse symbol" $ do
        "hello" `shouldParse` ExprSymbol "hello"
        "he/llo" `shouldParse` ExprSymbol "he/llo"
        "he.llo" `shouldParse` ExprSymbol "he.llo"
        ".hello" `shouldParse` ExprSymbol ".hello"
        ":hello" `shouldParse` ExprSymbol ":hello"
        "h#ello" `shouldParse` ExprSymbol "h#ello"
        "?hello" `shouldParse` ExprSymbol "?hello"
        "..." `shouldParse` ExprSymbol "..."
        "-" `shouldParse` ExprSymbol "-"
        "+" `shouldParse` ExprSymbol "+"
        "*" `shouldParse` ExprSymbol "*"
        "/" `shouldParse` ExprSymbol "/"
        "\\" `shouldParse` ExprSymbol "\\"
        "|" `shouldParse` ExprSymbol "|"
        "(foobar? !wow)" `shouldParse` ExprSymList [ExprSymbol "foobar?", ExprSymbol "!wow"]
      context "whe symbol is invalid" $ do
        it "fails" $ do
          (parseParsable "#hello" :: ParserResult Expr) `shouldSatisfy` isLeft

    context "when input is a number" $ do
      it "parses number" $ do
        "0.0" `shouldParse` ExprLiteral (LitNumber 0)
        -- "-208.3142" `shouldParse` ExprLiteral (LitNumber (-208.3142))
        -- "-208" `shouldParse` ExprLiteral (LitNumber (-208))
        -- "-208." `shouldParse` ExprLiteral (LitNumber (-208))
        "208" `shouldParse` ExprLiteral (LitNumber 208)
        "208." `shouldParse` ExprLiteral (LitNumber 208)
        "208.0" `shouldParse` ExprLiteral (LitNumber 208)
        "208.3142" `shouldParse` ExprLiteral (LitNumber 208.3142)

    context "when input is a string literal" $ do
      it "parses string" $ do
        [i| "" |] `shouldParse` ExprLiteral (LitString "")
        [i| "hjell !@#$%'''^&*()__+{}|:<>? ao" |] `shouldParse` ExprLiteral (LitString "hjell !@#$%'''^&*()__+{}|:<>? ao")
      context "when string literal is not closed" $ do
        it "fails" $ do
          (parseParsable [i|"|] :: ParserResult Expr) `shouldSatisfy` isLeft
          (parseParsable [i|"hello "world"|] :: ParserResult Expr) `shouldSatisfy` isLeft

    context "when input is quoted symbol" $ do
      it "parses quoted symbol" $ do
        "'hello" `shouldParse` ExprQuoted (ExprSymbol "hello")
        "'hello'world" `shouldParse` ExprQuoted (ExprSymbol "hello'world")
        "'foo!bar/wow" `shouldParse` ExprQuoted (ExprSymbol "foo!bar/wow")

    context "when input is quoted number" $ do
      it "parses quoted number" $ do
        "'345.4" `shouldParse` ExprQuoted (ExprLiteral $ LitNumber 345.4)

    context "when input contains single line comments" $ do
      it "ignores comments" $ do
        "#nil ; Some comment" `shouldParse` ExprLiteral LitNil
        [i|
          ; Start comment
          (foobar
            2 ; comment on this line
            (+ 2 3) ; comment on this line
          ) ; End comment
          |]
          `shouldParse` ExprSymList
            [ ExprSymbol "foobar",
              ExprLiteral $ LitNumber 2,
              ExprSymList [ExprSymbol "+", ExprLiteral $ LitNumber 2, ExprLiteral $ LitNumber 3]
            ]
    context "when input contains multi line comments" $ do
      it "ignores comments" $ do
        [i|
          #| start
          comment
           |#
          (foobar
            2 #| comment on this line |#
            (+ 2 3) #| comment on this line
            and this one
            |#
          ) #|
          end comment
           |#
             |]
          `shouldParse` ExprSymList
            [ ExprSymbol "foobar",
              ExprLiteral $ LitNumber 2,
              ExprSymList [ExprSymbol "+", ExprLiteral $ LitNumber 2, ExprLiteral $ LitNumber 3]
            ]

    context "when input is quoted s-expr" $ do
      it "parses quoted s-expr" $ do
        [i|'(2 (3 4) "wow")|]
          `shouldParse` ExprQuoted
            ( ExprSymList
                [ ExprLiteral $ LitNumber 2,
                  ExprSymList [ExprLiteral $ LitNumber 3, ExprLiteral $ LitNumber 4],
                  ExprLiteral $ LitString "wow"
                ]
            )

    context "when input is quoted string" $ do
      it "parses quoted string" $ do
        [i|'"hello world"|] `shouldParse` ExprQuoted (ExprLiteral $ LitString "hello world")
        [i|'"foo'bar"|] `shouldParse` ExprQuoted (ExprLiteral $ LitString "foo'bar")

    context "when input is s-expression" $ do
      it "parses expr successfully" $ do
        "()" `shouldParse` ExprSymList []
        "[]" `shouldParse` ExprSymList []
        "{}" `shouldParse` ExprSymList []
        "(#nil #T #F)"
          `shouldParse` ExprSymList [ExprLiteral LitNil, ExprLiteral $ LitBool True, ExprLiteral $ LitBool False]
        "[#nil #T #F]"
          `shouldParse` ExprSymList [ExprLiteral LitNil, ExprLiteral $ LitBool True, ExprLiteral $ LitBool False]
        "{#nil #T #F}"
          `shouldParse` ExprSymList [ExprLiteral LitNil, ExprLiteral $ LitBool True, ExprLiteral $ LitBool False]
        [i|(    #nil    42    "hello"  nice   #F   )|]
          `shouldParse` ExprSymList
            [ ExprLiteral LitNil,
              ExprLiteral $ LitNumber 42,
              ExprLiteral $ LitString "hello",
              ExprSymbol "nice",
              ExprLiteral $ LitBool False
            ]

    context "when input is nested symbolic expression" $ do
      it "parses expr successfully" $ do
        [i|(#nil (foobar? (#nil hello.world "hello world") #F))|]
          `shouldParse` ExprSymList
            [ ExprLiteral LitNil,
              ExprSymList
                [ ExprSymbol "foobar?",
                  ExprSymList [ExprLiteral LitNil, ExprSymbol "hello.world", ExprLiteral $ LitString "hello world"],
                  ExprLiteral $ LitBool False
                ]
            ]

        [i|(#nil (foobar? [#nil hello.world "hello world"] #F))|]
          `shouldParse` ExprSymList
            [ ExprLiteral LitNil,
              ExprSymList
                [ ExprSymbol "foobar?",
                  ExprSymList [ExprLiteral LitNil, ExprSymbol "hello.world", ExprLiteral $ LitString "hello world"],
                  ExprLiteral $ LitBool False
                ]
            ]
  describe "parse [Expr]" $ do
    it "parses expr successfully" $ do
      [i|
        (if (= value 200)
            "hello"
            (str+ "foo" "bar"))

        (runstuff '[2 45 2 3] { (foo 2) })
      |]
        `shouldParse` [ ExprSymList
                          [ ExprSymbol "if",
                            ExprSymList
                              [ ExprSymbol "=",
                                ExprSymbol "value",
                                ExprLiteral (LitNumber 200.0)
                              ],
                            ExprLiteral (LitString "hello"),
                            ExprSymList [ExprSymbol "str+", ExprLiteral (LitString "foo"), ExprLiteral (LitString "bar")]
                          ],
                        ExprSymList
                          [ ExprSymbol "runstuff",
                            ExprQuoted
                              ( ExprSymList
                                  [ ExprLiteral (LitNumber 2.0),
                                    ExprLiteral (LitNumber 45.0),
                                    ExprLiteral (LitNumber 2.0),
                                    ExprLiteral (LitNumber 3.0)
                                  ]
                              ),
                            ExprSymList [ExprSymList [ExprSymbol "foo", ExprLiteral (LitNumber 2.0)]]
                          ]
                      ]
