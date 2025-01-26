module Aether.Runtime.Value where

import Aether.Types
import Data.Maybe (fromMaybe)
import Text.Read (readMaybe)

valToNumber :: EvalValue -> Double
valToNumber (ValNumber num) = num
valToNumber (ValString num) = fromMaybe 0 $ readMaybe num
valToNumber (ValBool True) = 1
valToNumber (ValQuoted (ExprLiteral _ (LitNumber n))) = n
valToNumber (ValQuoted (ExprLiteral _ (LitString str))) = read str
valToNumber _ = 0

valToString :: EvalValue -> String
valToString (ValString s) = s
valToString (ValQuoted e) = exprToString e
valToString v = showCode v

valToBool :: EvalValue -> Bool
valToBool (ValBool bool) = bool
valToBool _ = True

exprToString :: Expr -> String
exprToString (ExprSymList _ ls) = unwords (map showCode ls)
exprToString (ExprSymbol _ s) = s
exprToString expr = showCode expr

subtractList :: (Num a) => [a] -> a
subtractList [] = 0
subtractList [x] = -x
subtractList (x : xs) = x - sum xs

divideList :: (Fractional a) => [a] -> a
divideList [] = 1
divideList (x : xs) = x / product xs

remainder2 :: (RealFrac a) => [a] -> Integer
remainder2 [a] = floor a
remainder2 (a : b : _) = floor a `mod` floor b
remainder2 _ = 0

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

mkResultVal :: EvalValue -> EvalValue -> EvalValue
mkResultVal e v = ValQuoted $ ExprSymList NullSpan [ExprValue e, ExprValue v]

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
evalErrorToValue (TypeError name message) = mkErrorVal (ValString "type-error") (ValString $ name ++ ": " ++ message)
evalErrorToValue e = ValQuoted $ ExprSymList NullSpan [ExprValue $ ValString "error", ExprValue $ ValString $ show e]

class LangShow a where
  showCode :: a -> String

instance LangShow EvalValue where
  showCode ValNil = "#nil"
  showCode (ValBool bool) = if bool then "#T" else "#F"
  showCode (ValString str) = str
  showCode (ValNumber n)
    | n == fromInteger (round n) = show (round n :: Int)
    | otherwise = show n
  showCode (ValLambda _ _ args body) = "(-> [" ++ unwords args ++ "]" ++ showCode body ++ ")"
  showCode (ValQuoted expr) = '\'' : showCode expr
  showCode (ValMacro {}) = "<macro>"
  showCode (ValBuiltin s) = "<builtin: " ++ s ++ ">"

instance (LangShow a) => LangShow [a] where
  showCode = unwords . map showCode

instance LangShow Expr where
  showCode (ExprLiteral _ (LitString s)) = s
  showCode (ExprLiteral _ (LitNumber n)) = showCode $ ValNumber n
  showCode (ExprLiteral _ (LitBool b)) = showCode $ ValBool b
  showCode (ExprLiteral _ LitNil) = showCode ValNil
  showCode (ExprSymList _ ls) = "(" ++ unwords (map showCode ls) ++ ")"
  showCode (ExprQuoted _ quote) = '\'' : showCode quote
  showCode (ExprSpliced _ expr) = ",@" ++ showCode expr
  showCode (ExprUnquoted _ expr) = ',' : showCode expr
  showCode (ExprSymbol _ s) = "<symbol: " ++ s ++ ">"
  showCode (ExprValue v) = showCode v
