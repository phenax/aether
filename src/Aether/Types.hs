module Aether.Types where

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
