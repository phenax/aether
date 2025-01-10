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

argToLabel :: Expr -> Name
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

showEvalValue :: EvalValue -> String
showEvalValue ValNil = "#nil"
showEvalValue (ValBool bool) = if bool then "#T" else "#F"
showEvalValue (ValString str) = str
showEvalValue (ValNumber n) = show n
showEvalValue (ValLambda _ args body) = "(-> [" ++ unwords args ++ "]" ++ showExpr body ++ ")"
showEvalValue (ValQuoted expr) = '\'' : showExpr expr
showEvalValue (ValMacro {}) = "<macro>"
showEvalValue (ValBuiltin s) = "<builtin: " ++ s ++ ">"

showExpr :: Expr -> String
showExpr (ExprLiteral (LitString s)) = s
showExpr (ExprLiteral (LitNumber n)) = showEvalValue $ ValNumber n
showExpr (ExprLiteral (LitBool b)) = showEvalValue $ ValBool b
showExpr (ExprLiteral LitNil) = showEvalValue ValNil
showExpr (ExprSymList ls) = "(" ++ unwords (map showExpr ls) ++ ")"
showExpr (ExprQuoted quote) = '\'' : showExpr quote
showExpr (ExprSpliced expr) = ",@" ++ showExpr expr
showExpr (ExprUnquoted expr) = ',' : showExpr expr
showExpr (ExprSymbol s) = "<symbol: " ++ s ++ ">"
showExpr (ExprValue v) = showEvalValue v
