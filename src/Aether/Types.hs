module Aether.Types where

import qualified Data.Map as Map
import Language.Haskell.TH.Lift (Lift, deriveLift, deriveLiftMany)
import qualified Text.Megaparsec as P

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

type Scope = Map.Map String EvalValue

newtype EvalEnvironment = EvalEnvironment {envCallStack :: [Scope]}
  deriving (Show, Eq)

instance Semigroup EvalEnvironment where
  (<>) a b = EvalEnvironment {envCallStack = envCallStack a <> envCallStack b}

instance Monoid EvalEnvironment where
  mempty = EvalEnvironment {envCallStack = [Map.empty]}

$(deriveLiftMany [''Literal, ''Expr, ''EvalValue, ''EvalError, ''EvalEnvironment])
