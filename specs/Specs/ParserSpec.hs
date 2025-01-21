module Specs.ParserSpec where

import Aether.Syntax.Parser
import Aether.Types
import Data.Either (isLeft)
import Data.String.Interpolate.IsString
import Test.Hspec
import TestUtils
import Text.Megaparsec (SourcePos (SourcePos), mkPos)

test :: SpecWith ()
test = do
  describe "parse Expr" $ do
    context "when input is boolean literal" $ do
      it "parses boolean successfully" $ do
        "#T" `shouldParse` ExprLiteral dummySpan (LitBool True)
        "#F" `shouldParse` ExprLiteral dummySpan (LitBool False)

    context "when input is nil literal" $ do
      it "parses boolean successfully" $ do
        "#nil" `shouldParse` ExprLiteral dummySpan LitNil

    context "when input is a symbol" $ do
      it "parse symbol" $ do
        "hello" `shouldParse` ExprSymbol dummySpan "hello"
        "he/llo" `shouldParse` ExprSymbol dummySpan "he/llo"
        "he.llo" `shouldParse` ExprSymbol dummySpan "he.llo"
        ".hello" `shouldParse` ExprSymbol dummySpan ".hello"
        ":hello" `shouldParse` ExprSymbol dummySpan ":hello"
        "h#ello" `shouldParse` ExprSymbol dummySpan "h#ello"
        "?hello" `shouldParse` ExprSymbol dummySpan "?hello"
        "..." `shouldParse` ExprSymbol dummySpan "..."
        "-" `shouldParse` ExprSymbol dummySpan "-"
        "+" `shouldParse` ExprSymbol dummySpan "+"
        "*" `shouldParse` ExprSymbol dummySpan "*"
        "/" `shouldParse` ExprSymbol dummySpan "/"
        "\\" `shouldParse` ExprSymbol dummySpan "\\"
        "|" `shouldParse` ExprSymbol dummySpan "|"
        "(foobar?     !wow 2)"
          `shouldParse` ExprSymList
            dummySpan
            [ ExprSymbol dummySpan "foobar?",
              ExprSymbol dummySpan "!wow",
              ExprLiteral dummySpan $ LitNumber 2
            ]
      context "whe symbol is invalid" $ do
        it "fails" $ do
          (parseParsable "#hello" :: ParserResult Expr) `shouldSatisfy` isLeft

    context "when input is a number" $ do
      it "parses number" $ do
        "0.0" `shouldParse` ExprLiteral dummySpan (LitNumber 0)
        "208" `shouldParse` ExprLiteral dummySpan (LitNumber 208)
        "208." `shouldParse` ExprLiteral dummySpan (LitNumber 208)
        "208.0" `shouldParse` ExprLiteral dummySpan (LitNumber 208)
        "208.3142" `shouldParse` ExprLiteral dummySpan (LitNumber 208.3142)

    context "when input is a string literal" $ do
      it "parses string" $ do
        [i| "" |] `shouldParse` ExprLiteral dummySpan (LitString "")
        [i| "hjell !@#$%'''^&*()__+{}|:<>? ao" |] `shouldParse` ExprLiteral dummySpan (LitString "hjell !@#$%'''^&*()__+{}|:<>? ao")
      context "when string literal is not closed" $ do
        it "fails" $ do
          (parseParsable [i|"|] :: ParserResult Expr) `shouldSatisfy` isLeft
          (parseParsable [i|"hello "world"|] :: ParserResult Expr) `shouldSatisfy` isLeft

    context "when input is quoted symbol" $ do
      it "parses quoted symbol" $ do
        "'hello" `shouldParse` ExprQuoted dummySpan (ExprSymbol dummySpan "hello")
        "'hello'world" `shouldParse` ExprQuoted dummySpan (ExprSymbol dummySpan "hello'world")
        "'foo!bar/wow" `shouldParse` ExprQuoted dummySpan (ExprSymbol dummySpan "foo!bar/wow")

    context "when input is quoted number" $ do
      it "parses quoted number" $ do
        "'345.4" `shouldParse` ExprQuoted dummySpan (ExprLiteral dummySpan $ LitNumber 345.4)

    context "when input contains single line comments" $ do
      it "ignores comments" $ do
        "#nil ; Some comment" `shouldParse` ExprLiteral dummySpan LitNil
        [i|
          ; Start comment
          (foobar
            2 ; comment on this line
            (+ 2 3) ; comment on this line
          ) ; End comment
          |]
          `shouldParse` ExprSymList
            dummySpan
            [ ExprSymbol dummySpan "foobar",
              ExprLiteral dummySpan $ LitNumber 2,
              ExprSymList
                dummySpan
                [ ExprSymbol dummySpan "+",
                  ExprLiteral dummySpan $ LitNumber 2,
                  ExprLiteral dummySpan $ LitNumber 3
                ]
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
            dummySpan
            [ ExprSymbol dummySpan "foobar",
              ExprLiteral dummySpan $ LitNumber 2,
              ExprSymList dummySpan [ExprSymbol dummySpan "+", ExprLiteral dummySpan $ LitNumber 2, ExprLiteral dummySpan $ LitNumber 3]
            ]

    context "when input is quoted s-expr" $ do
      it "parses quoted s-expr" $ do
        [i|'(2 (3 4) "wow")|]
          `shouldParse` ExprQuoted
            dummySpan
            ( ExprSymList
                dummySpan
                [ ExprLiteral dummySpan $ LitNumber 2,
                  ExprSymList dummySpan [ExprLiteral dummySpan $ LitNumber 3, ExprLiteral dummySpan $ LitNumber 4],
                  ExprLiteral dummySpan $ LitString "wow"
                ]
            )

    context "when input is quoted string" $ do
      it "parses quoted string" $ do
        [i|'"hello world"|] `shouldParse` ExprQuoted dummySpan (ExprLiteral dummySpan $ LitString "hello world")
        [i|'"foo'bar"|] `shouldParse` ExprQuoted dummySpan (ExprLiteral dummySpan $ LitString "foo'bar")

    context "when input is s-expression" $ do
      it "parses expr successfully" $ do
        "()" `shouldParse` ExprSymList dummySpan []
        "[]" `shouldParse` ExprSymList dummySpan []
        "{}" `shouldParse` ExprSymList dummySpan []
        "(#nil #T #F)"
          `shouldParse` ExprSymList dummySpan [ExprLiteral dummySpan LitNil, ExprLiteral dummySpan $ LitBool True, ExprLiteral dummySpan $ LitBool False]
        "[#nil #T #F]"
          `shouldParse` ExprSymList dummySpan [ExprLiteral dummySpan LitNil, ExprLiteral dummySpan $ LitBool True, ExprLiteral dummySpan $ LitBool False]
        "{#nil #T #F}"
          `shouldParse` ExprSymList dummySpan [ExprLiteral dummySpan LitNil, ExprLiteral dummySpan $ LitBool True, ExprLiteral dummySpan $ LitBool False]
        [i|(    #nil    42    "hello"  nice   #F   )|]
          `shouldParse` ExprSymList
            dummySpan
            [ ExprLiteral dummySpan LitNil,
              ExprLiteral dummySpan $ LitNumber 42,
              ExprLiteral dummySpan $ LitString "hello",
              ExprSymbol dummySpan "nice",
              ExprLiteral dummySpan $ LitBool False
            ]

    context "when input is nested symbolic expression" $ do
      it "parses expr successfully" $ do
        [i|(#nil (foobar? (#nil hello.world "hello world") #F))|]
          `shouldParse` ExprSymList
            dummySpan
            [ ExprLiteral dummySpan LitNil,
              ExprSymList
                dummySpan
                [ ExprSymbol dummySpan "foobar?",
                  ExprSymList dummySpan [ExprLiteral dummySpan LitNil, ExprSymbol dummySpan "hello.world", ExprLiteral dummySpan $ LitString "hello world"],
                  ExprLiteral dummySpan $ LitBool False
                ]
            ]

        [i|(#nil (foobar? [#nil hello.world "hello world"] #F))|]
          `shouldParse` ExprSymList
            dummySpan
            [ ExprLiteral dummySpan LitNil,
              ExprSymList
                dummySpan
                [ ExprSymbol dummySpan "foobar?",
                  ExprSymList dummySpan [ExprLiteral dummySpan LitNil, ExprSymbol dummySpan "hello.world", ExprLiteral dummySpan $ LitString "hello world"],
                  ExprLiteral dummySpan $ LitBool False
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
                          dummySpan
                          [ ExprSymbol dummySpan "if",
                            ExprSymList
                              dummySpan
                              [ ExprSymbol dummySpan "=",
                                ExprSymbol dummySpan "value",
                                ExprLiteral dummySpan (LitNumber 200.0)
                              ],
                            ExprLiteral dummySpan (LitString "hello"),
                            ExprSymList dummySpan [ExprSymbol dummySpan "str+", ExprLiteral dummySpan (LitString "foo"), ExprLiteral dummySpan (LitString "bar")]
                          ],
                        ExprSymList
                          dummySpan
                          [ ExprSymbol dummySpan "runstuff",
                            ExprQuoted
                              dummySpan
                              ( ExprSymList
                                  dummySpan
                                  [ ExprLiteral dummySpan (LitNumber 2.0),
                                    ExprLiteral dummySpan (LitNumber 45.0),
                                    ExprLiteral dummySpan (LitNumber 2.0),
                                    ExprLiteral dummySpan (LitNumber 3.0)
                                  ]
                              ),
                            ExprSymList dummySpan [ExprSymList dummySpan [ExprSymbol dummySpan "foo", ExprLiteral dummySpan (LitNumber 2.0)]]
                          ]
                      ]
