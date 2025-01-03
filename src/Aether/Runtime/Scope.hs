module Aether.Runtime.Scope where

import Aether.Types
import Control.Monad.RWS (MonadState (get, put))
import qualified Data.Map as Map

defineInCurrentScope :: String -> EvalValue -> EvalEnvironment -> EvalEnvironment
defineInCurrentScope name value env@(EvalEnvironment {envCallStack = (headStack : rest)}) =
  env {envCallStack = Map.insert name value headStack : rest}
defineInCurrentScope _ _ env = env

insertNewScope :: Scope -> EvalEnvironment -> EvalEnvironment
insertNewScope calls env = env {envCallStack = calls : envCallStack env}

lookupSymbol :: String -> EvalEnvironment -> Maybe EvalValue
lookupSymbol sym env@(EvalEnvironment {envCallStack = (syms : stack)}) =
  case Map.lookup sym syms of
    Just x -> Just x
    Nothing -> lookupSymbol sym $ env {envCallStack = stack}
lookupSymbol _ _ = Nothing

withSandboxedScope :: Evaluator m a -> Evaluator m a
withSandboxedScope run = do
  oldScope <- get
  res <- run
  put oldScope
  pure res

argsToScope :: [String] -> [Expr] -> (Expr -> Evaluator m EvalValue) -> Evaluator m Scope
argsToScope labels argsE expToVal = do
  args <- mapM expToVal argsE
  pure $ Map.fromList $ labels `zip` args
