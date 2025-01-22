module Aether.Runtime.Value where

import Aether.Types

valToNumber :: EvalValue -> Double
valToNumber (ValNumber num) = num
valToNumber (ValString num) = read num
valToNumber (ValBool True) = 1
valToNumber (ValQuoted (ExprLiteral _ (LitNumber n))) = n
valToNumber (ValQuoted (ExprLiteral _ (LitString str))) = read str
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
checkIfEqual (ValQuoted (ExprSymList _ [])) ValNil = True
checkIfEqual ValNil (ValQuoted (ExprSymList _ [])) = True
checkIfEqual (ValQuoted (ExprSymbol _ v1)) (ValQuoted (ExprSymbol _ v2)) = v1 == v2
checkIfEqual _ _ = False

argToLabel :: Expr -> Name
argToLabel (ExprSymbol _ sym) = sym
argToLabel _ = "_"

typeOfValue :: EvalValue -> String
typeOfValue (ValBool _) = "boolean"
typeOfValue (ValBuiltin _) = "function"
typeOfValue (ValLambda {}) = "function"
typeOfValue (ValMacro {}) = "macro"
typeOfValue (ValNumber _) = "number"
typeOfValue (ValQuoted (ExprSymList _ _)) = "list"
typeOfValue (ValQuoted (ExprSymbol _ _)) = "symbol"
typeOfValue (ValQuoted _) = "quote"
typeOfValue (ValString _) = "string"
typeOfValue ValNil = "list"

mkErrorVal :: EvalValue -> EvalValue -> EvalValue
mkErrorVal label msg = ValQuoted $ ExprSymList NullSpan [ExprValue label, ExprValue msg]

evalErrorToValue :: EvalError -> EvalValue
evalErrorToValue (UserError label msg) = mkErrorVal label msg
evalErrorToValue (NameNotFound name) =
  mkErrorVal
    (ValQuoted $ ExprSymbol NullSpan "symbol-not-found")
    (ValString message)
  where
    message = "Symbol '" ++ name ++ " is not defined"
evalErrorToValue (ArgumentLengthError strict expected got name) =
  mkErrorVal
    (ValQuoted $ ExprSymbol NullSpan "incorrect-argument-length")
    (ValString message)
  where
    message = "Expected " ++ expectedStr ++ " arguments but got " ++ show got ++ " (" ++ name ++ ")"
    expectedStr = (if strict then "" else "at least ") ++ show expected
evalErrorToValue e = ValQuoted $ ExprSymList NullSpan [ExprValue $ ValString "error", ExprValue $ ValString $ show e]

showEvalValue :: EvalValue -> String
showEvalValue ValNil = "#nil"
showEvalValue (ValBool bool) = if bool then "#T" else "#F"
showEvalValue (ValString str) = str
showEvalValue (ValNumber n) = show n
showEvalValue (ValLambda _ _ args body) = "(-> [" ++ unwords args ++ "]" ++ showExpr body ++ ")"
showEvalValue (ValQuoted expr) = '\'' : showExpr expr
showEvalValue (ValMacro {}) = "<macro>"
showEvalValue (ValBuiltin s) = "<builtin: " ++ s ++ ">"

showExpr :: Expr -> String
showExpr (ExprLiteral _ (LitString s)) = s
showExpr (ExprLiteral _ (LitNumber n)) = showEvalValue $ ValNumber n
showExpr (ExprLiteral _ (LitBool b)) = showEvalValue $ ValBool b
showExpr (ExprLiteral _ LitNil) = showEvalValue ValNil
showExpr (ExprSymList _ ls) = "(" ++ unwords (map showExpr ls) ++ ")"
showExpr (ExprQuoted _ quote) = '\'' : showExpr quote
showExpr (ExprSpliced _ expr) = ",@" ++ showExpr expr
showExpr (ExprUnquoted _ expr) = ',' : showExpr expr
showExpr (ExprSymbol _ s) = "<symbol: " ++ s ++ ">"
showExpr (ExprValue v) = showEvalValue v
