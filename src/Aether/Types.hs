module Aether.Types where

import Control.Monad.Except (MonadError)
import Control.Monad.RWS (MonadState)
import qualified Data.Map as Map
import Language.Haskell.TH.Lift (deriveLiftMany)

data Literal
  = LitString String
  | LitNumber Double
  | LitBool Bool
  | LitNil
  deriving (Show, Eq)

data Expr
  = ExprLiteral Literal
  | ExprQuoted Expr
  | ExprSymList [Expr]
  | ExprSymbol String
  | ExprUnquoted Expr
  | ExprValue EvalValue
  deriving (Show, Eq)

data EvalValue
  = ValBool Bool
  | ValLambda [String] Expr
  | ValMacro [String] Expr
  | ValNil
  | ValNumber Double
  | ValQuoted Expr
  | ValString String
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

type Evaluator m a = (MonadState EvalEnvironment m, MonadError EvalError m) => m a

$(deriveLiftMany [''Literal, ''Expr, ''EvalValue, ''EvalError, ''EvalEnvironment])
