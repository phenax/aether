module Specs.Integration.StdlibSpec where

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

  describe "core > math" $ do
    context "with overridden symbol for addition" $ do
      it "evaluates successfully" $ do
        evalExpr
          [i|
            (define (binary-op op a b) (op a b))
            (binary-op + 8 34)
            (+ 8 34)
          |]
          `shouldReturn` Right [ValNil, ValNumber 42.0, ValNumber 42.0]

    context "with overridden symbol for subtraction" $ do
      it "evaluates successfully" $ do
        evalExpr
          [i|
            (define (binary-op op a b) (op a b))
            (binary-op - 8 34)
            (- 8 34 (- 3) 2)
          |]
          `shouldReturn` Right [ValNil, ValNumber (-26.0), ValNumber (-25.0)]

  describe "core > if" $ do
    it "evaluates conditionals" $ do
      evalExpr
        [i|
          (if (gt? 99 0) (+ 8 1) (+ 2 2))
          (if (lt? 99 0) (+ 8 1) (+ 2 2))
        |]
        `shouldReturn` Right [ValNumber 9, ValNumber 4]

  describe "core > fold" $ do
    it "folds list of items" $ do
      evalExpr
        [i|
          (fold + 0 '[1 2 3 4 5 6])
          (fold (->[acc x] (+ (- acc) x)) 20 '[5 3])
          (fold + 0 2)
        |]
        `shouldReturn` Right [ValNumber 21, ValNumber 18, ValNumber 2]

  describe "core > reverse" $ do
    it "reverses the list of items" $ do
      evalExpr
        [i|
          (reverse '[1 2 3])
          (reverse '[])
        |]
        `shouldReturn` Right
          [ ValQuoted (ExprSymList [ExprValue (ValNumber 3), ExprValue (ValNumber 2), ExprValue (ValNumber 1)]),
            ValNil
          ]

  describe "core > map" $ do
    it "maps over list of items" $ do
      evalExpr
        [i|
          (map (-> [x] (* x 2)) '[1 2 3])
          (map (-> [x] x) '[])
        |]
        `shouldReturn` Right
          [ ValQuoted (ExprSymList [ExprValue (ValNumber 2), ExprValue (ValNumber 4), ExprValue (ValNumber 6)]),
            ValNil
          ]

  describe "core > null?" $ do
    context "when list is empty" $ do
      it "returns true" $ do
        evalExpr [i| (null? '()) |] `shouldReturn` Right [ValBool True]
        evalExpr [i| (null? #nil) |] `shouldReturn` Right [ValBool True]

    context "when list is not empty" $ do
      it "returns true" $ do
        evalExpr [i| (null? '(1)) |] `shouldReturn` Right [ValBool False]

  describe "core > list" $ do
    it "constructs a list" $ do
      evalExpr
        [i|
          (list 1 2 3 4 5 6)
          (list)
        |]
        `shouldReturn` Right
          [ ValQuoted
              ( ExprSymList
                  [ ExprValue (ValNumber 1),
                    ExprValue (ValNumber 2),
                    ExprValue (ValNumber 3),
                    ExprValue (ValNumber 4),
                    ExprValue (ValNumber 5),
                    ExprValue (ValNumber 6)
                  ]
              ),
            ValQuoted $ ExprSymList []
          ]

  describe "core > concat" $ do
    it "concatenates 2 lists" $ do
      evalExpr
        [i|
          (concat [list 1 2 3] [list 4 5 6])
          (concat '[] [list 4 5 6])
          (concat [list 1 2 3] '[])
        |]
        `shouldReturn` Right
          [ ValQuoted $
              ExprSymList
                [ ExprValue (ValNumber 1),
                  ExprValue (ValNumber 2),
                  ExprValue (ValNumber 3),
                  ExprValue (ValNumber 4),
                  ExprValue (ValNumber 5),
                  ExprValue (ValNumber 6)
                ],
            ValQuoted $
              ExprSymList
                [ ExprValue (ValNumber 4),
                  ExprValue (ValNumber 5),
                  ExprValue (ValNumber 6)
                ],
            ValQuoted $
              ExprSymList
                [ ExprValue (ValNumber 1),
                  ExprValue (ValNumber 2),
                  ExprValue (ValNumber 3)
                ]
          ]

  describe "core > let" $ do
    it "creates a binding within the given scope" $ do
      evalExpr
        [i|
          (let [
            (hello 2)
            (foobar 3)
            (dooger 9) ]
            (+ foobar hello dooger))
        |]
        `shouldReturn` Right [ValNumber 14]

  describe "core > cond" $ do
    it "allows building up conditionals" $ do
      evalExpr
        [i|
          (set num 50)
          (cond
            [(lt? num 3)   "baby"]
            [(lt? num 13)  "child"]
            [(lt? num 18)  "adolescent"]
            [(lt? num 60)  "adult"]
            [#T            "almost dead"])
        |]
        `shouldReturn` Right [ValNil, ValString "adult"]

  describe "core > length" $ do
    it "returns length of list" $ do
      evalExpr
        [i|
          (length (list 1 2 3 4 5))
        |]
        `shouldReturn` Right [ValNumber 5]

    context "when list is empty" $ do
      it "returns length of list" $ do
        evalExpr
          [i|
            (length (list))
            (length #nil)
            (length '[])
          |]
          `shouldReturn` Right [ValNumber 0, ValNumber 0, ValNumber 0]

  describe "core > apply" $ do
    it "applies args to a function" $ do
      evalExpr
        [i|
          (define (foobar a b c) (+ a (* b c)))
          (set arguments '[1 2 3])
          (apply foobar arguments)
        |]
        `shouldReturn` Right [ValNil, ValNil, ValNumber 7]

  describe "core > id" $ do
    it "returns given value" $ do
      evalExpr [i| (id 5) |] `shouldReturn` Right [ValNumber 5]

  describe "core > const" $ do
    it "returns first arg" $ do
      evalExpr [i| ((const 20) 99) |] `shouldReturn` Right [ValNumber 20]

  describe "core > infix" $ do
    it "allows using infix notation evaluated left to right" $ do
      evalExpr
        [i|
          ($ 2 * ($ 5 + 2))
          <$ 5 + <$ 9 - 3> * 2>
        |]
        `shouldReturn` Right [ValNumber 14, ValNumber 22]

  describe "core > range" $ do
    it "generates a range of numbers" $ do
      evalExpr
        [i|
          (range 2 5)
          (range (- 2) 3)
          (range 5 5)
        |]
        `shouldReturn` Right
          [ ValQuoted $
              ExprSymList
                [ ExprValue (ValNumber 2),
                  ExprValue (ValNumber 3),
                  ExprValue (ValNumber 4),
                  ExprValue (ValNumber 5)
                ],
            ValQuoted $
              ExprSymList
                [ ExprValue (ValNumber (-2)),
                  ExprValue (ValNumber (-1)),
                  ExprValue (ValNumber 0),
                  ExprValue (ValNumber 1),
                  ExprValue (ValNumber 2),
                  ExprValue (ValNumber 3)
                ],
            ValQuoted $ ExprSymList [ExprValue (ValNumber 5)]
          ]

    context "when start is greater than end" $ do
      it "returns an empty list" $ do
        evalExpr
          [i|
            (range 5 2)
            (range (- 2) (- 3))
          |]
          `shouldReturn` Right [ValNil, ValNil]

  describe "builtin > type" $ do
    it "returns correct types for given values" $ do
      evalExpr
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
        `shouldReturn` Right
          [ ValQuoted (ExprSymbol "list"),
            ValQuoted (ExprSymbol "list"),
            ValQuoted (ExprSymbol "number"),
            ValQuoted (ExprSymbol "string"),
            ValQuoted (ExprSymbol "symbol"),
            ValQuoted (ExprSymbol "quote"),
            ValQuoted (ExprSymbol "boolean"),
            ValQuoted (ExprSymbol "list"),
            ValQuoted (ExprSymbol "function"),
            ValQuoted (ExprSymbol "function"),
            ValQuoted (ExprSymbol "macro")
          ]
