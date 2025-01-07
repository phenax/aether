module Aether.Types where

import Control.Monad.Except (MonadError)
import Control.Monad.RWS.Strict (MonadState)
import Data.List (intercalate)
import qualified Data.Map.Strict as Map
import Language.Haskell.TH.Lift (deriveLiftMany)

type Name = String

data Literal
  = LitBool !Bool
  | LitNil
  | LitNumber !Double
  | LitString !String
  deriving (Show, Eq)

data Expr
  = ExprLiteral !Literal
  | ExprQuoted !Expr
  | ExprSymList ![Expr]
  | ExprSymbol !Name
  | ExprUnquoted !Expr
  | ExprSpliced !Expr
  | ExprValue !EvalValue
  deriving (Show, Eq)

data EvalValue
  = ValBool !Bool
  | ValLambda !Stack ![String] !Expr
  | ValMacro !Stack ![String] !Expr
  | ValBuiltin Name
  | ValNil
  | ValNumber !Double
  | ValQuoted !Expr
  | ValString !String
  deriving (Show, Eq)

data EvalError
  = ArgumentError String
  | TypeError String
  | NameNotFound Name
  | UnknownError String
  deriving (Show, Eq)

data Scope = Scope {scopeId :: !ScopeId, scopeTable :: !(Map.Map String EvalValue)}
  deriving (Show, Eq)

instance Semigroup Scope where
  (<>) s1 s2 = Scope {scopeId = scopeId s1, scopeTable = Map.union (scopeTable s1) (scopeTable s2)}

newtype Stack = Stack {stack :: [Scope]}

instance Show Stack where
  show (Stack st) = "<stack: " ++ intercalate "|" (map ((\(ScopeId i) -> show i) . scopeId) st) ++ ">"

-- NOTE: To not affect lambdas in test. DO NOT CHECK FOR EQUALITY
instance Eq Stack where
  (==) _ _ = True

newtype ScopeId = ScopeId Int
  deriving (Show, Eq)

data EvalEnvironment = EvalEnvironment {envCallStack :: !Stack, envScopeId :: !Int}
  deriving (Show)

instance Semigroup EvalEnvironment where
  (<>) _ _ = undefined -- TODO: Think

instance Monoid EvalEnvironment where
  mempty = EvalEnvironment {envScopeId = 1, envCallStack = Stack [Scope {scopeId = ScopeId 0, scopeTable = Map.empty}]}

type Evaluator m a = (MonadState EvalEnvironment m, MonadError EvalError m) => m a

$(deriveLiftMany [''Literal, ''Expr, ''EvalValue, ''EvalError, ''ScopeId, ''Scope, ''Stack])
