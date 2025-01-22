module Aether.Runtime.Scope where

import Aether.Types
import Control.Monad.Error.Class (MonadError (throwError))
import Control.Monad.RWS (gets, modify')
import Data.Bifunctor (Bifunctor (second))
import qualified Data.Map.Strict as Map

defineInCurrentScope :: Name -> EvalValue -> EvalEnvironment -> EvalEnvironment
defineInCurrentScope name value env@(EvalEnvironment {envCallStack = Stack (scope : rest)}) =
  env {envCallStack = Stack $ scope {scopeTable = Map.insert name value (scopeTable scope)} : rest}
defineInCurrentScope _ _ env = env

updateSymbolValue :: Name -> EvalValue -> EvalEnvironment -> EvalEnvironment
updateSymbolValue name value env =
  case updateSymbolValueInStack $ envCallStack env of
    Just stack -> env {envCallStack = stack}
    Nothing -> defineInCurrentScope name value env
  where
    updateSymbolValueInStack (Stack (scope : rest)) = do
      case Map.lookup name $ scopeTable scope of
        Just _ -> Just $ Stack (updatedScope : rest)
          where
            updatedScope = scope {scopeTable = Map.insert name value $ scopeTable scope}
        Nothing -> (\st -> st {stack = scope : stack st}) <$> updateSymbolValueInStack (Stack rest)
    updateSymbolValueInStack (Stack []) = Nothing

lookupSymbol :: Name -> EvalEnvironment -> Maybe EvalValue
lookupSymbol sym env@(EvalEnvironment {envCallStack = Stack (scope : stack)}) =
  case Map.lookup sym (scopeTable scope) of
    Just x -> Just x
    Nothing -> lookupSymbol sym $ env {envCallStack = Stack stack}
lookupSymbol _ _ = Nothing

mkScope :: Map.Map Name EvalValue -> Evaluator m Scope
mkScope table = do
  scopeId <- gets envScopeId
  modify' $ \env -> env {envScopeId = scopeId + 1}
  pure $ Scope {scopeId = ScopeId scopeId, scopeTable = table}

showScope :: Scope -> String
showScope (Scope {scopeId = ScopeId sid}) | sid == 0 = "    <root-scope>"
showScope (Scope {scopeId = ScopeId sid, scopeTable}) =
  "    <id: " ++ show sid ++ "> " ++ unwords (Map.keys scopeTable)

showStack :: Stack -> String
showStack (Stack stack) = unlines $ map showScope stack

closure :: (Show a) => Scope -> Stack -> Evaluator m a -> Evaluator m a
closure targetScope (Stack {stack = closureStack}) eval = do
  !oldStack <- gets (stack . envCallStack)
  modify' mergeStack
  !res <- eval
  modify' $ popStack oldStack
  pure res
  where
    targetStack = targetScope : closureStack

    popStack oldStack env = env {envCallStack = Stack closureScopePoppedStack}
      where
        currentStack = stack $ envCallStack env
        closureScopePoppedStack = reverse $ selectMatchingScopes (reverse oldStack) (reverse currentStack)

    mergeStack env = env {envCallStack = Stack mergedStack}
      where
        currentStack = stack $ envCallStack env
        mergedStack = reverse $ mergeCommonScopes (reverse targetStack) (reverse currentStack)

    mergeCommonScopes (x1 : xs1) (x2 : xs2) | scopeId x1 == scopeId x2 = (x1 <> x2) : mergeCommonScopes xs1 xs2
    mergeCommonScopes xs _ = xs

    selectMatchingScopes (x1 : xs1) (x2 : xs2) | scopeId x1 == scopeId x2 = x2 : selectMatchingScopes xs1 xs2
    selectMatchingScopes xs _ = xs

zipArgs :: [Name] -> [EvalValue] -> Int -> (Int, Maybe [(Name, EvalValue)])
zipArgs [] _ argCount = (argCount, Just [])
zipArgs ["...", label] rest argCount = (argCount, Just [(label, ValQuoted . ExprSymList NullSpan . fmap ExprValue $ rest)])
zipArgs (label : labels) (arg : args) argCount = second (fmap ((label, arg) :)) $ zipArgs labels args (argCount + 1)
zipArgs _ _ argCount = (argCount, Nothing)

argsToScope :: [Name] -> [EvalValue] -> Evaluator m Scope
argsToScope labels args = do
  case zipArgs labels args 0 of
    (_, Just zargs) -> mkScope $ Map.fromList zargs
    (argCount, Nothing) -> throwError $ ArgumentLengthError False argCount (length args) "<todo: name of fn>"
