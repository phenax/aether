module Specs.Integration.StdlibSpec where

import Aether.Runtime (runInterpreter)
import Aether.Runtime.Value (mkErrorVal)
import Aether.Syntax.Parser
import Aether.Types
import Data.String.Interpolate.IsString
import Test.Hspec
import Text.Megaparsec (errorBundlePretty)

test :: SpecWith ()
test = describe "stdlib" $ do
  let evalExpr code = do
        let results = parseAll "input" code
        case results of
          Right exprs -> runInterpreter exprs
          Left e -> error $ errorBundlePretty e

  describe "#math" $ do
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

  describe "#if" $ do
    it "evaluates conditionals" $ do
      evalExpr
        [i|
          (if (gt? 99 0) (+ 8 1) (+ 2 2))
          (if (lt? 99 0) (+ 8 1) (+ 2 2))
        |]
        `shouldReturn` Right [ValNumber 9, ValNumber 4]

  describe "#fold" $ do
    it "folds list of items" $ do
      evalExpr
        [i|
          (fold + 0 '[1 2 3 4 5 6])
          (fold (->[acc x] (+ (- acc) x)) 20 '[5 3])
          (fold + 0 2)
        |]
        `shouldReturn` Right [ValNumber 21, ValNumber 18, ValNumber 2]

  describe "#reverse" $ do
    it "reverses the list of items" $ do
      evalExpr
        [i|
          (reverse '[1 2 3])
          (reverse '[])
        |]
        `shouldReturn` Right
          [ ValQuoted (ExprSymList NullSpan [ExprValue (ValNumber 3), ExprValue (ValNumber 2), ExprValue (ValNumber 1)]),
            ValNil
          ]

  describe "#map" $ do
    it "maps over list of items" $ do
      evalExpr
        [i|
          (map (-> [x] (* x 2)) '[1 2 3])
          (map (-> [x] x) '[])
        |]
        `shouldReturn` Right
          [ ValQuoted (ExprSymList NullSpan [ExprValue (ValNumber 2), ExprValue (ValNumber 4), ExprValue (ValNumber 6)]),
            ValNil
          ]

  describe "#for" $ do
    it "iterates over list of items" $ do
      evalExpr
        [i|
          (set out '[])
          (for '[1 2 3] {-> [x]
            (let [ (double (* x 2)) ]
              (set out (concat out double)))
          })
          out
        |]
        `shouldReturn` Right
          [ ValNil,
            ValNil,
            ValQuoted $ ExprSymList NullSpan [ExprValue (ValNumber 2), ExprValue (ValNumber 4), ExprValue (ValNumber 6)]
          ]

  describe "#each" $ do
    it "iterates over list of items" $ do
      evalExpr
        [i|
          (set out '[])
          (each
            {-> [x] (set out (concat out (* x 2)))}
            '[1 2 3])
          out
        |]
        `shouldReturn` Right
          [ ValNil,
            ValNil,
            ValQuoted $ ExprSymList NullSpan [ExprValue (ValNumber 2), ExprValue (ValNumber 4), ExprValue (ValNumber 6)]
          ]

  describe "#null?" $ do
    context "when list is empty" $ do
      it "returns true" $ do
        evalExpr [i| (null? '()) |] `shouldReturn` Right [ValBool True]
        evalExpr [i| (null? #nil) |] `shouldReturn` Right [ValBool True]

    context "when list is not empty" $ do
      it "returns true" $ do
        evalExpr [i| (null? '(1)) |] `shouldReturn` Right [ValBool False]

  describe "#list" $ do
    it "constructs a list" $ do
      evalExpr
        [i|
          (list 1 2 3 4 5 6)
          (list)
        |]
        `shouldReturn` Right
          [ ValQuoted
              ( ExprSymList
                  NullSpan
                  [ ExprValue (ValNumber 1),
                    ExprValue (ValNumber 2),
                    ExprValue (ValNumber 3),
                    ExprValue (ValNumber 4),
                    ExprValue (ValNumber 5),
                    ExprValue (ValNumber 6)
                  ]
              ),
            ValQuoted $ ExprSymList NullSpan []
          ]

  describe "#concat" $ do
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
                NullSpan
                [ ExprValue (ValNumber 1),
                  ExprValue (ValNumber 2),
                  ExprValue (ValNumber 3),
                  ExprValue (ValNumber 4),
                  ExprValue (ValNumber 5),
                  ExprValue (ValNumber 6)
                ],
            ValQuoted $
              ExprSymList
                NullSpan
                [ ExprValue (ValNumber 4),
                  ExprValue (ValNumber 5),
                  ExprValue (ValNumber 6)
                ],
            ValQuoted $
              ExprSymList
                NullSpan
                [ ExprValue (ValNumber 1),
                  ExprValue (ValNumber 2),
                  ExprValue (ValNumber 3)
                ]
          ]

  describe "#let" $ do
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

  describe "#cond" $ do
    it "allows building up conditionals" $ do
      evalExpr
        [i|
          (define num 50)
          (cond
            [(lt? num 3)   "baby"]
            [(lt? num 13)  "child"]
            [(lt? num 18)  "adolescent"]
            [(lt? num 60)  "adult"]
            [#T            "almost dead"])
        |]
        `shouldReturn` Right [ValNil, ValString "adult"]

  describe "#length" $ do
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

  describe "#apply" $ do
    it "applies args to a function" $ do
      evalExpr
        [i|
          (define (foobar a b c) (+ a (* b c)))
          (define arguments '[1 2 3])
          (apply foobar arguments)
        |]
        `shouldReturn` Right [ValNil, ValNil, ValNumber 7]

  describe "#id" $ do
    it "returns given value" $ do
      evalExpr [i| (id 5) |] `shouldReturn` Right [ValNumber 5]

  describe "#const" $ do
    it "returns first arg" $ do
      evalExpr [i| ((const 20) 99) |] `shouldReturn` Right [ValNumber 20]

  describe "#infix" $ do
    it "allows using infix notation evaluated left to right" $ do
      evalExpr
        [i|
          ($ 2 * ($ 5 + 2))
          ($ ($ 9 - 3) * 2)
        |]
        `shouldReturn` Right [ValNumber 14, ValNumber 12]

  describe "#range" $ do
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
                NullSpan
                [ ExprValue (ValNumber 2),
                  ExprValue (ValNumber 3),
                  ExprValue (ValNumber 4),
                  ExprValue (ValNumber 5)
                ],
            ValQuoted $
              ExprSymList
                NullSpan
                [ ExprValue (ValNumber (-2)),
                  ExprValue (ValNumber (-1)),
                  ExprValue (ValNumber 0),
                  ExprValue (ValNumber 1),
                  ExprValue (ValNumber 2),
                  ExprValue (ValNumber 3)
                ],
            ValQuoted $ ExprSymList NullSpan [ExprValue (ValNumber 5)]
          ]

    context "when start is greater than end" $ do
      it "returns an empty list" $ do
        evalExpr
          [i|
            (range 5 2)
            (range (- 2) (- 3))
          |]
          `shouldReturn` Right [ValNil, ValNil]

  describe "#record" $ do
    it "creates constructor and property getters for given record" $ do
      evalExpr
        [i|
          (record Person
            :name
            :age
            :gender
            :address/country)
          (define john (Person "John" 25 'male "India"))
          (:name john)
          (:gender john)
          (:address/country john)
          (:age john)
        |]
        `shouldReturn` Right
          [ ValNil,
            ValNil,
            ValString "John",
            ValQuoted $ ExprSymbol NullSpan "male",
            ValString "India",
            ValNumber 25
          ]

  describe "#elem-at" $ do
    it "returns element at index in a list" $ do
      evalExpr
        [i|
          (elem-at 1 (list 99 38 2 23))
          (elem-at 0 (list 99 38 2 23))
          (elem-at 99 (list 99 38 2 23))
        |]
        `shouldReturn` Right
          [ ValNumber 38,
            ValNumber 99,
            ValNil
          ]

    context "when index is out of bounds" $ do
      it "returns nil" $ do
        evalExpr
          [i|
            (elem-at 99 (list 99 38 2 23))
            (elem-at (- 1) (list 99 38 2 23))
            |]
          `shouldReturn` Right [ValNil, ValNil]

  describe "#zip-with" $ do
    it "returns a lists zipped with given function" $ do
      evalExpr
        [i|
          (zip-with
            tuple
            (list 1 2 3)
            (list 4 5 6))
        |]
        `shouldReturn` Right
          [ ValQuoted $
              ExprSymList
                NullSpan
                [ ExprValue $ ValQuoted $ ExprSymList NullSpan [ExprValue $ ValNumber 1, ExprValue $ ValNumber 4],
                  ExprValue $ ValQuoted $ ExprSymList NullSpan [ExprValue $ ValNumber 2, ExprValue $ ValNumber 5],
                  ExprValue $ ValQuoted $ ExprSymList NullSpan [ExprValue $ ValNumber 3, ExprValue $ ValNumber 6]
                ]
          ]

    context "when list lengths are different" $ do
      it "returns the smallest number of items zipped in the list" $ do
        evalExpr
          [i|
            (zip-with tuple (list 1 2) (list 4 5 6))
            (zip-with tuple (list 1 2 3) (list 4 5))
          |]
          `shouldReturn` Right
            [ ValQuoted $
                ExprSymList
                  NullSpan
                  [ ExprValue $ ValQuoted $ ExprSymList NullSpan [ExprValue $ ValNumber 1, ExprValue $ ValNumber 4],
                    ExprValue $ ValQuoted $ ExprSymList NullSpan [ExprValue $ ValNumber 2, ExprValue $ ValNumber 5]
                  ],
              ValQuoted $
                ExprSymList
                  NullSpan
                  [ ExprValue $ ValQuoted $ ExprSymList NullSpan [ExprValue $ ValNumber 1, ExprValue $ ValNumber 4],
                    ExprValue $ ValQuoted $ ExprSymList NullSpan [ExprValue $ ValNumber 2, ExprValue $ ValNumber 5]
                  ]
            ]

  describe "#index-of" $ do
    it "returns the index of given item" $ do
      evalExpr
        [i|
          (define ls (list 1 1 2 3 5 8 13 21))
          (index-of 8 ls)
          (index-of 21 ls)
          (index-of 1 ls)
        |]
        `shouldReturn` Right [ValNil, ValNumber 5, ValNumber 7, ValNumber 0]

    context "when item is not in list" $ do
      it "returns nil" $ do
        evalExpr
          [i| (index-of 182 (list 1 1 2 3 5 8 13 21)) |]
          `shouldReturn` Right [ValNil]

  describe "#contains?" $ do
    it "returns the index of given item" $ do
      evalExpr
        [i|
          (define ls (list 1 1 2 3 5 8 13 21))
          (contains? 8 ls)
          (contains? 1 ls)
          (contains? 99 ls)
        |]
        `shouldReturn` Right [ValNil, ValBool True, ValBool True, ValBool False]

  describe "#|>" $ do
    it "pipes a value through a list of functions" $ do
      evalExpr
        [i|
          (|> 20 (curry + 1) (curry * 2) (curry (flip -) 1))
          {$ 20
            |> (curry + 1)
            |> (curry * 2)
            |> (curry (flip -) 1)}
        |]
        `shouldReturn` Right [ValNumber 41, ValNumber 41]

  describe "#Error/Result" $ do
    context "when try returns an error result" $ do
      it "allow accessing error with getters" $ do
        evalExpr
          [i|
            (define res (try invalid-symbol))
            (:result/value res)
            (:result/error res)
          |]
          `shouldReturn` Right
            [ ValNil,
              ValNil,
              mkErrorVal
                (ValQuoted $ ExprSymbol NullSpan "symbol-not-found")
                (ValString "Symbol 'invalid-symbol is not defined")
            ]
      it "allow accessing error label and message with getters" $ do
        evalExpr
          [i|
            (define res (try invalid-symbol))
            (:error/label (:result/error res))
            (:error/message (:result/error res))
          |]
          `shouldReturn` Right
            [ ValNil,
              ValQuoted $ ExprSymbol NullSpan "symbol-not-found",
              ValString "Symbol 'invalid-symbol is not defined"
            ]
    context "when try returns a value result" $ do
      it "allow accessing value with getters" $ do
        evalExpr
          [i|
            (define res (try 200))
            (:result/value res)
            (:result/error res)
          |]
          `shouldReturn` Right
            [ ValNil,
              ValNumber 200,
              ValNil
            ]

  describe "#expand" $ do
    it "expands/destructures the values into symbols" $ do
      evalExpr
        [i|
          (expand [a1 a2 a3] (list 1 2 3))
          (list a1 a2 a3)

          (expand [a1 a2 a3] (list 1 2))
          (list a1 a2 a3)

          (expand [a1 a2 a3] (list 1 2 3))
          (list a1 a2)
        |]
        `shouldReturn` Right
          [ ValNil,
            ValQuoted $ ExprSymList NullSpan [ExprValue (ValNumber 1.0), ExprValue (ValNumber 2.0), ExprValue (ValNumber 3.0)],
            ValNil,
            ValQuoted $ ExprSymList NullSpan [ExprValue (ValNumber 1.0), ExprValue (ValNumber 2.0), ExprValue ValNil],
            ValNil,
            ValQuoted $ ExprSymList NullSpan [ExprValue (ValNumber 1.0), ExprValue (ValNumber 2.0)]
          ]

    context "when value is empty" $ do
      it "expands/destructures the values into symbols" $ do
        evalExpr
          [i|
            (expand [a1 a2] '())
            (list a1 a2)
          |]
          `shouldReturn` Right
            [ ValNil,
              ValQuoted $ ExprSymList NullSpan [ExprValue ValNil, ExprValue ValNil]
            ]

    context "when value is not a list" $ do
      it "expands/destructures the values into symbols" $ do
        evalExpr
          [i|
            (expand [a1 a2] 500)
            (list a1 a2)

            (expand [a1 a2] "hello")
            (list a1 a2)
          |]
          `shouldReturn` Right
            [ ValNil,
              ValQuoted $ ExprSymList NullSpan [ExprValue $ ValNumber 500, ExprValue ValNil],
              ValNil,
              ValQuoted $ ExprSymList NullSpan [ExprValue $ ValString "hello", ExprValue ValNil]
            ]
