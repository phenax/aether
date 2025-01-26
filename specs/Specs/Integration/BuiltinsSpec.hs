module Specs.Integration.BuiltinsSpec where

import qualified Aether.Runtime.Interpreter as Runtime
import Aether.Runtime.Value (mkErrorVal, mkResultVal)
import Aether.Syntax.Parser
import Aether.Types
import Data.String.Interpolate.IsString
import Data.Text (Text)
import System.Exit (ExitCode (ExitFailure, ExitSuccess))
import Test.HMock (ExpectContext (expect), (|->))
import Test.Hspec
import TestUtils
import Text.Megaparsec (errorBundlePretty)

evalExpr :: Text -> Evaluator m [EvalValue]
evalExpr code = do
  let results = parseAll "input" code
  case results of
    Right exprs -> mapM Runtime.interpretExpression exprs
    Left e -> error $ errorBundlePretty e

test :: SpecWith ()
test = describe "builtin" $ do
  describe "#type" $ do
    it "returns correct types for given values" $ do
      let code =
            [i|
          (type '(1 2 3))
          (type '())
          (type 200)
          (type "hello")
          (type 'hello)
          (type '"hello world")
          (type #T)
          (type #nil)
          (type (-> [] 20))
          (type +)
          (type if)
        |]
      result <- runWithMocks $ evalExpr code

      result
        `shouldBe` Right
          [ ValQuoted (ExprSymbol NullSpan "list"),
            ValQuoted (ExprSymbol NullSpan "list"),
            ValQuoted (ExprSymbol NullSpan "number"),
            ValQuoted (ExprSymbol NullSpan "string"),
            ValQuoted (ExprSymbol NullSpan "symbol"),
            ValQuoted (ExprSymbol NullSpan "quote"),
            ValQuoted (ExprSymbol NullSpan "boolean"),
            ValQuoted (ExprSymbol NullSpan "list"),
            ValQuoted (ExprSymbol NullSpan "function"),
            ValQuoted (ExprSymbol NullSpan "function"),
            ValQuoted (ExprSymbol NullSpan "macro")
          ]

  describe "#error!/try" $ do
    context "when expression raises an error" $ do
      it "returns result with error" $ do
        let code =
              [i|
            ; Have to use (quote 'division) instead of 'division due to a bug in macros
            ; TODO: Fix that and replace it with 'division
            (define (divide! a b)
              (if (= b 0)
                (error! (quote 'division-by-zero) "You divided by zero and died")
                (/ a b)))

            (try invalid-symbol)
            (try (error! 'hello "World"))
            (try (divide! 5 0))
            (try (lt? 1 2 3))
          |]
        result <- runWithMocks $ evalExpr code

        result
          `shouldBe` Right
            [ ValNil,
              mkResultVal
                ( mkErrorVal
                    (ValQuoted $ ExprSymbol NullSpan "symbol-not-found")
                    (ValString "Symbol 'invalid-symbol is not defined")
                )
                ValNil,
              mkResultVal
                ( mkErrorVal
                    (ValQuoted $ ExprSymbol NullSpan "hello")
                    (ValString "World")
                )
                ValNil,
              mkResultVal
                ( mkErrorVal
                    (ValQuoted $ ExprSymbol NullSpan "division-by-zero")
                    (ValString "You divided by zero and died")
                )
                ValNil,
              mkResultVal
                ( mkErrorVal
                    (ValQuoted $ ExprSymbol NullSpan "incorrect-argument-length")
                    (ValString "Expected 2 arguments but got 3 (lt?)")
                )
                ValNil
            ]
    context "when expression does not raise an error" $ do
      it "returns result without error" $ do
        let code =
              [i|
            (try 20)
            (try (+ 2 5))
            (try (lt? (+ 3 4) 2))
          |]
        result <- runWithMocks $ evalExpr code

        result
          `shouldBe` Right
            [ mkResultVal ValNil $ ValNumber 20,
              mkResultVal ValNil $ ValNumber 7,
              mkResultVal ValNil $ ValBool False
            ]

  describe "#progn" $ do
    context "evaluates expression sequentially in current scope" $ do
      it "defines value in current scope" $ do
        let code =
              [i|
            (define foobar 10)
            foobar
            (progn
              (define foobar (+ foobar 5))
              foobar)
            foobar
          |]
        result <- runWithMocks $ evalExpr code

        result
          `shouldBe` Right
            [ValNil, ValNumber 10, ValNumber 15, ValNumber 15]

  describe "#define" $ do
    context "when defining value" $ do
      it "defines value in current scope" $ do
        let code =
              [i|
            (define foobar "outside")
            (define (scope)
              (define foobar "inside lambda")
              foobar)

            (do
              (define foobar "inside do")
              foobar)
            (scope)
            foobar
          |]
        result <- runWithMocks $ evalExpr code

        result
          `shouldBe` Right
            [ ValNil,
              ValNil,
              ValString "inside do",
              ValString "inside lambda",
              ValString "outside"
            ]

    context "when defining lambda" $ do
      it "lambda uses scope where it was defined" $ do
        let code =
              [i|
            (define (bar foobar) (foobar 5))
            (define (foo foobar) (bar (-> [x] (+ foobar x))))
            (foo 200)
          |]
        result <- runWithMocks $ evalExpr code

        result
          `shouldBe` Right
            [ValNil, ValNil, ValNumber 205]

    context "when defining lambda with variable number of arguments" $ do
      it "allows accepting a variable number of arguments" $ do
        let code =
              [i|
            (define (foo a b c ... rest)
              (list a b c rest))
            (foo 1 2 3 4 5 6 7)
          |]
        result <- runWithMocks $ evalExpr code

        result
          `shouldBe` Right
            [ ValNil,
              ValQuoted $
                ExprSymList
                  dummySpan
                  [ ExprValue (ValNumber 1),
                    ExprValue (ValNumber 2),
                    ExprValue (ValNumber 3),
                    ExprValue $
                      ValQuoted $
                        ExprSymList
                          dummySpan
                          [ ExprValue (ValNumber 4),
                            ExprValue (ValNumber 5),
                            ExprValue (ValNumber 6),
                            ExprValue (ValNumber 7)
                          ]
                  ]
            ]

  describe "#set" $ do
    context "when setting a new value" $ do
      it "defines value in current scope" $ do
        let code =
              [i|
            (set a "outside")

            (do
              (set b "inside")
              b)
            a
            (try b)
          |]
        result <- runWithMocks $ evalExpr code

        result
          `shouldBe` Right
            [ ValNil,
              ValString "inside",
              ValString "outside",
              mkResultVal
                ( mkErrorVal
                    (ValQuoted $ ExprSymbol NullSpan "symbol-not-found")
                    (ValString "Symbol 'b is not defined")
                )
                ValNil
            ]

    context "when updating a value set in the scope above" $ do
      it "updates value in the previous scope" $ do
        let code =
              [i|
            (set foobar "outside")
            (define (scope)
              (set foobar "inside lambda")
              foobar)

            foobar
            (do
              (set foobar "inside do")
              foobar)
            (scope)
            foobar
          |]
        result <- runWithMocks $ evalExpr code

        result
          `shouldBe` Right
            [ ValNil,
              ValNil,
              ValString "outside",
              ValString "inside do",
              ValString "inside lambda",
              ValString "inside lambda"
            ]

  describe "#displayNl" $ do
    it "prints given arguments to screen" $ do
      let code = [i| (displayNl 42 5.2 "--" '(1 2)) |]
      result <- runWithMocks $ do
        expect $ PutStringToScreen "42" |-> ()
        expect $ PutStringToScreen "5.2" |-> ()
        expect $ PutStringToScreen "--" |-> ()
        expect $ PutStringToScreen "'(1 2)" |-> ()
        expect $ PutStringToScreen "\n" |-> ()
        evalExpr code
      result `shouldBe` Right [ValNil]

  describe "#!: exec commands" $ do
    it "runs given command" $ do
      let code =
            [i|
              (! ls -la /home/user ,(+ 5 2))
              (set cmd "hello")
              (set arg "my arg")
              (try (! ,cmd --flag something ,arg (1 2)))
            |]
      result <- runWithMocks $ do
        expect $ ExecCommand "ls" ["-la", "/home/user", "7"] |-> (ExitSuccess, "", "")
        expect $ ExecCommand "hello" ["--flag", "something", "my arg", "1 2"] |-> (ExitFailure 2, "", "Contents of stderror")
        evalExpr code
      result
        `shouldBe` Right
          [ ValQuoted (ExprSymList NullSpan [ExprValue (ValString ""), ExprValue (ValString "")]),
            ValNil,
            ValNil,
            mkResultVal
              ( mkErrorVal
                  (ValQuoted (ExprSymbol NullSpan "proc/non-zero-exit-code"))
                  (ValString "Process exited with status code 2\nContents of stderror")
              )
              ValNil
          ]

  describe "#import" $ do
    let mkTestScript s = [ExprSymList NullSpan [ExprSymbol NullSpan "displayNl", ExprLiteral NullSpan $ LitString s]]

    it "imports script files sequentially" $ do
      let code = [i| (import "./script1" 'script2) |]
      result <- runWithMocks $ do
        expect $ LoadScriptToAST "./script1" |-> Right (mkTestScript "Loaded script 1")
        expect $ LoadScriptToAST "script2" |-> Right (mkTestScript "Loaded script 2")
        expect $ PutStringToScreen "Loaded script 1"
        expect $ PutStringToScreen "\n"
        expect $ PutStringToScreen "Loaded script 2"
        expect $ PutStringToScreen "\n"
        evalExpr code
      result `shouldBe` Right [ValNil]

    context "when imported script has an error" $ do
      it "imports a script" $ do
        let code = [i| (try (import './script-with-error)) |]
        result <- runWithMocks $ do
          expect $ LoadScriptToAST "./script-with-error" |-> Left "Invalid script"
          evalExpr code
        result
          `shouldBe` Right
            [ mkResultVal
                (mkErrorVal (ValQuoted (ExprSymbol NullSpan "import-error")) (ValString "Invalid script"))
                ValNil
            ]

      it "stops importing the other scripts in the sequence" $ do
        let code = [i| (try (import 'script-with-error 'second-script)) |]
        result <- runWithMocks $ do
          expect $ LoadScriptToAST "script-with-error" |-> Left "Invalid script"
          evalExpr code
        result
          `shouldBe` Right
            [ mkResultVal
                (mkErrorVal (ValQuoted (ExprSymbol NullSpan "import-error")) (ValString "Invalid script"))
                ValNil
            ]

  describe "#get-args" $ do
    it "gets cli args" $ do
      let code = [i| (get-args) |]
      result <- runWithMocks $ do
        expect $ GetArgs |-> ["1", "2"]
        evalExpr code
      result `shouldBe` Right [ValQuoted $ ExprSymList NullSpan [ExprValue $ ValString "1", ExprValue $ ValString "2"]]

  describe "#string" $ do
    it "concatenates all values as string" $ do
      let code = [i| (string 1 2 "hello" 'world-symbol) |]
      result <- runWithMocks $ evalExpr code
      result `shouldBe` Right [ValString "12helloworld-symbol"]

  describe "#make-symbol" $ do
    it "converts value into a symbol" $ do
      let code = [i| (make-symbol "hello/world") (make-symbol 28) |]
      result <- runWithMocks $ evalExpr code
      result
        `shouldBe` Right
          [ ValQuoted $ ExprSymbol NullSpan "hello/world",
            ValQuoted $ ExprSymbol NullSpan "28"
          ]

    context "when number of args is not 1" $ do
      it "throws argument length error" $ do
        let code = [i| (make-symbol 1 2 "wow") |]
        result <- runWithMocks $ evalExpr code
        result `shouldBe` Left (ArgumentLengthError True 1 3 "make-symbol")

  describe "#exit" $ do
    it "exits with given status code" $ do
      let code = [i| (exit 2) (exit 0) (exit "hello") |]
      result <- runWithMocks $ do
        expect $ SystemExit 2
        expect $ SystemExit 0
        expect $ SystemExit 0
        evalExpr code
      result `shouldBe` Right [ValNil, ValNil, ValNil]
