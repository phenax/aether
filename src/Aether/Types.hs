module Aether.Types where

import qualified Data.Map as Map

data Literal
  = LitString String
  | LitNumber Double
  | LitBool Bool
  | LitNil
  deriving (Show, Eq)

data Expr
  = ExprSymList [Expr]
  | ExprSymbol String
  | ExprQuoted Expr
  | ExprLiteral Literal
  deriving (Show, Eq)

data EvalValue
  = ValString String
  | ValBool Bool
  | ValNumber Double
  | ValQuoted Expr
  | ValLambda [String] Expr
  | ValMacro [String] Expr
  | ValNil
  deriving (Show, Eq)

data EvalError
  = TypeError String
  | NameNotFound String
  | UnknownError String
  deriving (Show, Eq)
