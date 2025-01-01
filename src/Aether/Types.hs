module Aether.Types where

data Literal
  = LitString String
  | LitInt Int
  | LitBool Bool
  | LitNil
  | LitSymbol Expr
  deriving (Show, Eq)

data Expr
  = ExprSymList [Expr]
  | ExprIdentifier String
  | ExprLiteral Literal
  deriving (Show, Eq)
