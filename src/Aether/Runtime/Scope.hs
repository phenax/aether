module Aether.Runtime.Scope where

import Aether.Types
import Control.Monad.Error.Class (MonadError (throwError))
import Control.Monad.RWS (gets, modify')
import Data.Bifunctor (Bifunctor (second))
import Data.Foldable (Foldable (toList))
import qualified Data.Map.Strict as Map
import Data.Sequence (Seq (Empty, (:<|), (:|>)), (<|), (|>))

defineInCurrentScope :: Name -> EvalValue -> EvalEnvironment -> EvalEnvironment
defineInCurrentScope name value env@(EvalEnvironment {envCallStack = Stack (rest :|> scope)}) =
  env {envCallStack = updatedStack}
  where
    updatedStack = Stack $ rest |> scope {scopeTable = updatedScopeTable}
    updatedScopeTable = Map.insert name value (scopeTable scope)
defineInCurrentScope _ _ (EvalEnvironment {envCallStack = Stack Empty}) = error "Panic! stack empty"

updateSymbolValue :: Name -> EvalValue -> EvalEnvironment -> EvalEnvironment
updateSymbolValue name value env =
  case updateSymbolValueInStack $ stack $ envCallStack env of
    Just st -> env {envCallStack = Stack st}
    Nothing -> defineInCurrentScope name value env
  where
    updateSymbolValueInStack (rest :|> scope) =
      case Map.lookup name $ scopeTable scope of
        Just _ -> Just (rest |> updatedScope)
          where
            updatedScope = scope {scopeTable = Map.insert name value $ scopeTable scope}
        Nothing -> (|> scope) <$> updateSymbolValueInStack rest
    updateSymbolValueInStack Empty = Nothing

lookupSymbol :: Name -> EvalEnvironment -> Maybe EvalValue
lookupSymbol sym = lookupSymbol' . stack . envCallStack
  where
    lookupSymbol' (rest :|> scope) =
      case Map.lookup sym (scopeTable scope) of
        Just val -> Just val
        Nothing -> lookupSymbol' rest
    lookupSymbol' Empty = Nothing

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
showStack = unlines . toList . fmap showScope . stack

closure :: (Show a) => Scope -> Stack -> Evaluator m a -> Evaluator m a
closure targetScope (Stack {stack = closureStack}) eval = do
  !oldStack <- gets (stack . envCallStack)
  modify' mergeStack
  !res <- eval
  modify' $ popStack oldStack
  pure res
  where
    mergeStack env = env {envCallStack = Stack mergedStack}
      where
        targetStack = closureStack :|> targetScope
        currentStack = stack $ envCallStack env
        mergedStack = joinCommonScopes (<>) targetStack currentStack

    popStack oldStack env = env {envCallStack = Stack closureScopePoppedStack}
      where
        currentStack = stack $ envCallStack env
        closureScopePoppedStack = joinCommonScopes (\_ cur -> cur) oldStack currentStack

joinCommonScopes :: (Scope -> Scope -> Scope) -> Seq Scope -> Seq Scope -> Seq Scope
joinCommonScopes fn (x1 :<| xs1) (x2 :<| xs2)
  | scopeId x1 == scopeId x2 =
      fn x1 x2 <| joinCommonScopes fn xs1 xs2
joinCommonScopes _ xs1 _ = xs1

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
