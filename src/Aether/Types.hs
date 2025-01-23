{-# OPTIONS_GHC -Wno-orphans #-}

module Aether.Types where

import Control.Monad.Except (MonadError)
import Control.Monad.RWS.Strict (MonadIO (liftIO), MonadState)
import qualified Data.Map.Strict as Map
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Language.Haskell.TH.Lift (deriveLiftMany)
import Text.Megaparsec (Pos, SourcePos)

type Name = String

data SourceSpan = SourceSpan SourcePos SourcePos | NoSourceSpan | NullSpan
  deriving (Show)

-- NOTE: Temporary to not worry about tests
instance Eq SourceSpan where
  (==) NullSpan _ = True
  (==) _ NullSpan = True
  (==) NoSourceSpan NoSourceSpan = True
  (==) (SourceSpan a b) (SourceSpan c d) = a == b && c == d
  (==) _ _ = False

data Literal
  = LitBool !Bool
  | LitNil
  | LitNumber !Double
  | LitString !String
  deriving (Show, Eq)

data Expr
  = ExprLiteral !SourceSpan !Literal
  | ExprQuoted !SourceSpan !Expr
  | ExprSymList !SourceSpan ![Expr]
  | ExprSymbol !SourceSpan !Name
  | ExprUnquoted !SourceSpan !Expr
  | ExprSpliced !SourceSpan !Expr
  | ExprValue !EvalValue
  deriving (Show, Eq)

data EvalValue
  = ValBool !Bool
  | ValLambda !Stack !SourceSpan ![Name] !Expr
  | ValMacro !Stack !SourceSpan ![Name] !Expr
  | ValBuiltin Name
  | ValNil
  | ValNumber !Double
  | ValQuoted !Expr
  | ValString !String
  deriving (Show, Eq)

data EvalError
  = ArgumentError String
  | ArgumentLengthError Bool Int Int String
  | TypeError String
  | NameNotFound Name
  | UserError EvalValue EvalValue
  | UnknownError String
  deriving (Show, Eq)

data Scope = Scope {scopeId :: !ScopeId, scopeTable :: !(Map.Map Name EvalValue)}
  deriving (Show, Eq)

instance Semigroup Scope where
  (<>) s1 s2 = Scope {scopeId = scopeId s1, scopeTable = Map.union (scopeTable s1) (scopeTable s2)}

newtype Stack = Stack {stack :: Seq Scope}

instance Show Stack where
  show (Stack st) = "<stack: " ++ show (fmap showId st) ++ ">"
    where
      showId = (\(ScopeId i) -> show i) . scopeId

-- NOTE: To not affect lambdas in test. DO NOT CHECK FOR EQUALITY
instance Eq Stack where
  (==) _ _ = True

newtype ScopeId = ScopeId Int
  deriving (Show, Eq)

data EvalEnvironment = EvalEnvironment {envCallStack :: !Stack, envScopeId :: !Int}
  deriving (Show)

instance Semigroup EvalEnvironment where
  (<>) _ _ = undefined -- TODO: Think. Maybe merging of stacks?

instance Monoid EvalEnvironment where
  mempty =
    EvalEnvironment
      { envScopeId = 1,
        envCallStack = Stack $ Seq.singleton $ Scope {scopeId = ScopeId 0, scopeTable = Map.empty}
      }

class (Monad m) => MonadLangIO m where
  putStringToScreen :: String -> m ()

newtype LangIOT m a = LangIOT {runLangIOT :: m a}
  deriving (Functor, Applicative, Monad)

deriving instance (MonadState s m) => MonadState s (LangIOT m)

deriving instance (MonadError e m) => MonadError e (LangIOT m)

instance (MonadIO m) => MonadLangIO (LangIOT m) where
  putStringToScreen = LangIOT . liftIO . putStr

type Evaluator m a = (MonadState EvalEnvironment m, MonadError EvalError m, MonadLangIO m) => m a

$(deriveLiftMany [''Literal, ''Expr, ''EvalValue, ''EvalError, ''ScopeId, ''Scope, ''Stack, ''SourceSpan, ''SourcePos, ''Pos])
