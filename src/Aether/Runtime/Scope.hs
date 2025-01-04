module Aether.Runtime.Scope where

import Aether.Types
import Control.Monad.RWS (gets, modify')
import qualified Data.Map as Map

defineInCurrentScope :: String -> EvalValue -> EvalEnvironment -> EvalEnvironment
defineInCurrentScope name value env@(EvalEnvironment {envCallStack = Stack (scope : rest)}) =
  env {envCallStack = Stack $ scope {scopeTable = Map.insert name value (scopeTable scope)} : rest}
defineInCurrentScope _ _ env = env

lookupSymbol :: String -> EvalEnvironment -> Maybe EvalValue
lookupSymbol sym env@(EvalEnvironment {envCallStack = Stack (scope : stack)}) =
  case Map.lookup sym (scopeTable scope) of
    Just x -> Just x
    Nothing -> lookupSymbol sym $ env {envCallStack = Stack stack}
lookupSymbol _ _ = Nothing

mkScope :: Map.Map String EvalValue -> Evaluator m Scope
mkScope table = do
  scopeId <- gets envScopeId
  modify' $ \env -> env {envScopeId = scopeId + 1}
  pure $ Scope {scopeId = ScopeId scopeId, scopeTable = table}

closure :: Stack -> Evaluator m a -> Evaluator m a
closure (Stack {stack}) eval = do
  oldCallstack <- gets envCallStack
  modify' mergeStack
  res <- eval
  modify' $ \env -> env {envCallStack = oldCallstack}
  pure res
  where
    mergeStack env@(EvalEnvironment {envCallStack = Stack {stack = callsite}}) =
      env {envCallStack = Stack $ reverse $ mergeCommon (reverse stack) (reverse callsite)}
    mergeCommon (x1 : xs1) (x2 : xs2) | scopeId x1 == scopeId x2 = (x1 <> x2) : mergeCommon xs1 xs2
    mergeCommon xs _ = xs

argsToScope :: [String] -> [Expr] -> (Expr -> Evaluator m EvalValue) -> Evaluator m Scope
argsToScope labels argsE expToVal = do
  args <- mapM expToVal argsE
  mkScope $ Map.fromList $ labels `zip` args
