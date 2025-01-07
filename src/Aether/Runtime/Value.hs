module Aether.Runtime.Value where

import Aether.Types

valToNumber :: EvalValue -> Double
valToNumber (ValNumber num) = num
valToNumber (ValString num) = read num
valToNumber (ValBool True) = 1
valToNumber (ValQuoted (ExprLiteral (LitNumber n))) = n
valToNumber (ValQuoted (ExprLiteral (LitString str))) = read str
valToNumber _ = 0

valToBool :: EvalValue -> Bool
valToBool (ValBool bool) = bool
valToBool _ = True

checkIfAllEqual :: [EvalValue] -> Bool
checkIfAllEqual [] = False
checkIfAllEqual [_] = True
checkIfAllEqual (x1 : x2 : xs) = checkIfEqual x1 x2 && checkIfAllEqual (x2 : xs)

checkIfEqual :: EvalValue -> EvalValue -> Bool
checkIfEqual (ValNumber v1) (ValNumber v2) = v1 == v2
checkIfEqual (ValString v1) (ValString v2) = v1 == v2
checkIfEqual (ValBool v1) (ValBool v2) = v1 == v2
checkIfEqual ValNil ValNil = True
checkIfEqual (ValQuoted (ExprSymList [])) ValNil = True
checkIfEqual ValNil (ValQuoted (ExprSymList [])) = True
checkIfEqual (ValQuoted (ExprSymbol v1)) (ValQuoted (ExprSymbol v2)) = v1 == v2
checkIfEqual _ _ = False

argToLabel :: Expr -> String
argToLabel (ExprSymbol sym) = sym
argToLabel _ = "_"

typeOfValue :: EvalValue -> String
typeOfValue (ValBool _) = "boolean"
typeOfValue (ValBuiltin _) = "function"
typeOfValue (ValLambda {}) = "function"
typeOfValue (ValMacro {}) = "macro"
typeOfValue (ValNumber _) = "number"
typeOfValue (ValQuoted (ExprSymList _)) = "list"
typeOfValue (ValQuoted (ExprSymbol _)) = "symbol"
typeOfValue (ValQuoted _) = "quote"
typeOfValue (ValString _) = "string"
typeOfValue ValNil = "list"
