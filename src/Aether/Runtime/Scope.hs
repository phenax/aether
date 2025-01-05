module Aether.Runtime.Scope where

import Aether.Types
import Control.Monad.Error.Class (MonadError (throwError))
import Control.Monad.RWS (gets, modify')
import qualified Data.Map.Strict as Map
import qualified Debug.Trace as Debug

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

zipArgs :: [String] -> [EvalValue] -> Maybe [(String, EvalValue)]
zipArgs [] [] = pure []
zipArgs ["...", label] rest = pure [(label, ValQuoted . ExprSymList . fmap ExprValue $ rest)]
zipArgs (label : labels) (arg : args) = ((label, arg) :) <$> zipArgs labels args
zipArgs _ _ = Nothing

argsToScope :: [String] -> [Expr] -> (Expr -> Evaluator m EvalValue) -> Evaluator m Scope
argsToScope labels argsE expToVal = do
  args <- mapM expToVal argsE
  -- Debug.traceShowM labels
  -- Debug.traceShowM args
  case zipArgs labels args of
    Just zargs -> mkScope $ Map.fromList zargs
    Nothing -> throwError $ ArgumentError "Invalid number of arguments"
