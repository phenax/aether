module Aether.Runtime.Interpreter where

import Aether.Runtime.Builtins (builtins)
import Aether.Runtime.LangIO (LangIOT (runLangIOT))
import Aether.Runtime.Scope (argsToScope, closure, lookupSymbol)
import Aether.Runtime.Value (LangShow (showCode))
import Aether.Types
import Control.Monad.Except (ExceptT, MonadError (throwError), runExceptT)
import Control.Monad.State.Strict (MonadIO, StateT (runStateT), gets)
import qualified Data.Map.Strict as Map

class Interpretable a b where
  interpret :: a -> Evaluator m b

instance (Interpretable a b) => Interpretable [a] [b] where
  interpret = mapM interpret

instance Interpretable Literal EvalValue where
  interpret = \case
    LitString str -> pure $ ValString str
    LitBool bool -> pure $ ValBool bool
    LitNumber num -> pure $ ValNumber num
    LitNil -> pure ValNil

instance Interpretable Expr EvalValue where
  interpret = \case
    ExprLiteral _ lit -> interpret lit
    ExprSymbol _ sym -> do
      isBuiltin <- Map.member sym <$> builtins interpret
      let fallback
            | isBuiltin = pure $ ValBuiltin sym
            | otherwise = throwError $ NameNotFound sym
      gets (lookupSymbol sym) >>= maybe fallback pure
    ExprSymList _ [] -> pure ValNil
    ExprSymList _ (fnE : argsE) -> interpret fnE >>= (`evaluateCall` argsE)
    ExprUnquoted _ _expr -> undefined -- TODO: either throw error or `interpret expr`
    ExprSpliced _ _expr -> undefined -- same interpret expr
    -- ExprValue (ValQuoted expr) -> interpret expr
    ExprValue value -> pure value
    ExprQuoted _ (ExprSymList _ []) -> pure ValNil
    ExprQuoted _ quote -> ValQuoted <$> evalUnquotes quote
      where
        evalUnquotes :: Expr -> Evaluator m Expr
        evalUnquotes (ExprUnquoted _ expr) = ExprValue <$> interpret expr
        evalUnquotes (ExprSymList _ exprs) = ExprSymList NullSpan <$> (mapM evalUnquotes exprs >>= evalSplices)
        evalUnquotes expr = pure expr

        evalSplices :: [Expr] -> Evaluator m [Expr]
        evalSplices [] = pure []
        evalSplices (ExprSpliced _ expr : exprs) = do
          val <- interpret expr
          spliced <- evalSplices exprs
          (++ spliced) <$> expandSymList val
        evalSplices (expr : exprs) = (expr :) <$> evalSplices exprs

        expandSymList :: EvalValue -> Evaluator m [Expr]
        expandSymList (ValQuoted (ExprSymList _ exprs)) = pure exprs
        expandSymList (ValQuoted (ExprQuoted _ (ExprSymList _ exprs))) = pure exprs
        expandSymList (ValQuoted sym@(ExprSymbol _ _)) = interpret sym >>= expandSymList
        expandSymList ValNil = pure []
        expandSymList e = pure [ExprValue e]

interpretExpression :: Expr -> Evaluator m EvalValue
interpretExpression = interpret

evaluateCall :: EvalValue -> [Expr] -> Evaluator m EvalValue
-- Lambda
evaluateCall (ValLambda stack _ labels body) argsE = do
  args <- interpret argsE
  argsScope <- argsToScope labels args
  closure argsScope stack $ interpret body

-- Macros
-- TODO: Figure out how to scope macros
evaluateCall (ValMacro (Stack _defnstack) _ labels body) argsE = do
  callerstack <- gets envCallStack
  !args <- mapM interpretMacroArgs argsE
  !argsScope <- argsToScope labels args
  !result <- closure argsScope callerstack $ interpret body
  case result of
    ValQuoted expr -> interpret $ unwrapQuotes expr
    _ -> pure result
  where
    interpretMacroArgs :: Expr -> Evaluator m EvalValue
    interpretMacroArgs (ExprLiteral _ lit) = interpret lit
    interpretMacroArgs (ExprValue v) = pure v
    interpretMacroArgs (ExprQuoted _ quote) = pure $ ValQuoted quote
    interpretMacroArgs (ExprSymList _ exprs) = ValQuoted . ExprSymList NullSpan . fmap ExprValue <$> mapM interpretMacroArgs exprs
    interpretMacroArgs val = pure $ ValQuoted val

    unwrapQuotes :: Expr -> Expr
    unwrapQuotes (ExprSymList _ exprs) = ExprSymList NullSpan $ map unwrapQuotes exprs
    unwrapQuotes (ExprValue (ValQuoted (ExprSymList _ exprs))) = ExprSymList NullSpan $ map unwrapQuotes exprs
    unwrapQuotes (ExprValue (ValQuoted e)) = e
    unwrapQuotes e = e

-- Primitive evaluation
evaluateCall (ValBuiltin name) argsE = do
  builtin <- Map.lookup name <$> builtins interpret
  case builtin of
    Just eval -> eval argsE
    Nothing -> throwError $ UnknownError "Primtive not defined"

-- Quoted values
evaluateCall (ValQuoted quote) argsE = do
  !fn <- interpret quote
  evaluateCall fn argsE

-- Booleans are callable
evaluateCall (ValBool bool) argsE = do
  case argsE of
    (then_ : _) | bool -> interpret then_
    (_ : else_ : _) | not bool -> interpret else_
    _ -> pure ValNil

-- Invald call
evaluateCall value args = do
  throwError
    . TypeError "call"
    $ "Can't call value: " ++ showCode value ++ " with: " ++ showCode args

runEvaluator ::
  (MonadIO m) =>
  LangIOT (ExceptT EvalError (StateT EvalEnvironment m)) a ->
  EvalEnvironment ->
  m (Either EvalError a, EvalEnvironment)
runEvaluator m = runStateT (runExceptT $ runLangIOT m)
