module Aether.Runtime.Interpreter where

import Aether.Runtime.Scope (argsToScope, defineInCurrentScope, insertNewScope, lookupSymbol, withSandboxedScope)
import Aether.Runtime.Value
import Aether.Types
import Control.Monad.Except (ExceptT, MonadError (throwError), runExceptT)
import Control.Monad.State (MonadIO, StateT (runStateT), gets, modify')
import qualified Data.Map as Map

interpretLiteral :: Literal -> Evaluator m EvalValue
interpretLiteral = \case
  (LitString str) -> pure $ ValString str
  (LitBool bool) -> pure $ ValBool bool
  (LitNumber num) -> pure $ ValNumber num
  LitNil -> pure ValNil

interpretExpression :: Expr -> Evaluator m EvalValue
interpretExpression = \case
  (ExprLiteral lit) -> interpretLiteral lit
  (ExprQuoted (ExprSymList [])) -> pure ValNil
  (ExprQuoted quote) -> pure $ ValQuoted quote
  (ExprSymbol sym) -> gets (lookupSymbol sym) >>= maybe (throwError $ NameNotFound sym) pure
  (ExprSymList []) -> pure ValNil
  (ExprSymList (ExprSymbol name : argsE)) -> do
    let evalSymbol = interpretExpression (ExprSymbol name) >>= (`evaluateCall` argsE)
    evaluateBuiltins name argsE >>= maybe evalSymbol pure
  (ExprSymList (fnE : argsE)) -> interpretExpression fnE >>= (`evaluateCall` argsE)

evaluateBuiltins :: String -> [Expr] -> Evaluator m (Maybe EvalValue)
-- Set a value in current scope
evaluateBuiltins "set" [ExprSymbol name, valueE] = do
  value <- interpretExpression valueE
  modify' $ defineInCurrentScope name value
  pure $ Just value
evaluateBuiltins "set" _ = do throwError $ TypeError "Invalid call to set"

-- Lambda expression syntax
evaluateBuiltins "->" (ExprSymList argsE : body) = do
  let lambda = ValLambda (argToLabel <$> argsE) $ ExprSymList (ExprSymbol "do" : body)
  pure $ Just lambda
evaluateBuiltins "->" _ = do throwError $ TypeError "Invalid call to ->"

-- Define macro in current scope
evaluateBuiltins "defmacro" (ExprSymList (ExprSymbol name : argsE) : body) = do
  let macro = ValMacro (argToLabel <$> argsE) $ ExprSymList (ExprSymbol "do" : body)
  modify' $ defineInCurrentScope name macro
  pure $ Just ValNil
evaluateBuiltins "defmacro" _ = do throwError $ TypeError "Invalid call to defmacro"

-- Define lambda in current scope
evaluateBuiltins "define" (ExprSymList (ExprSymbol name : argsE) : body) = do
  let value = ValLambda (argToLabel <$> argsE) $ ExprSymList (ExprSymbol "do" : body)
  modify' $ defineInCurrentScope name value
  pure $ Just ValNil
evaluateBuiltins "define" _ = do throwError $ TypeError "Invalid call to define"

-- Do syntax (chained expressions)
evaluateBuiltins "do" body = do
  results <- withSandboxedScope $ do
    modify' $ insertNewScope Map.empty
    mapM interpretExpression body
  pure $ Just $ case results of
    [] -> ValNil
    _ -> last results

-- Evaluate a quoted expression
evaluateBuiltins "eval" [expr] = do
  res <- interpretExpression expr
  case res of
    ValQuoted quote -> Just <$> interpretExpression quote
    _ -> pure . Just $ res
evaluateBuiltins "eval" _ = do throwError $ TypeError "Invalid number of arguments sent to eval"

-- Math operations
evaluateBuiltins "+" exprs = Just <$> operateOnExprs (ValNumber . sum . fmap valToNumber) exprs
evaluateBuiltins "*" exprs = Just <$> operateOnExprs (ValNumber . product . fmap valToNumber) exprs
evaluateBuiltins "-" exprs = Just <$> operateOnExprs (ValNumber . subtractVal . fmap valToNumber) exprs
  where
    subtractVal [] = 0
    subtractVal [x] = -x
    subtractVal (x : xs) = x - sum xs
evaluateBuiltins "/" exprs = Just <$> operateOnExprs (ValNumber . divideVal . fmap valToNumber) exprs
  where
    divideVal [] = 1
    divideVal (x : xs) = x / product xs

-- Comparison operations
evaluateBuiltins "lt?" [exp1, exp2] = Just <$> numberBinaryOp (\a -> ValBool . (a <)) exp1 exp2
evaluateBuiltins "lt?" _ = throwError $ TypeError "Invalid number of arguments for lt?"
evaluateBuiltins "gt?" [exp1, exp2] = Just <$> numberBinaryOp (\a -> ValBool . (a >)) exp1 exp2
evaluateBuiltins "gt?" _ = throwError $ TypeError "Invalid number of arguments for gt?"
evaluateBuiltins "lte?" [exp1, exp2] = Just <$> numberBinaryOp (\a -> ValBool . (a <=)) exp1 exp2
evaluateBuiltins "lte?" _ = throwError $ TypeError "Invalid number of arguments for lte?"
evaluateBuiltins "gte?" [exp1, exp2] = Just <$> numberBinaryOp (\a -> ValBool . (a >=)) exp1 exp2
evaluateBuiltins "gte?" _ = throwError $ TypeError "Invalid number of arguments for gte?"
evaluateBuiltins "eq?" exprs = Just <$> operateOnExprs (ValBool . checkIfAllEqual) exprs
-- Boolean operations
evaluateBuiltins "not" [expr] = Just . ValBool . not . valToBool <$> interpretExpression expr
evaluateBuiltins "not" _ = throwError $ TypeError "Invalid number of arguments for not"
evaluateBuiltins "&&" exprs = Just <$> operateOnExprs (ValBool . all valToBool) exprs
evaluateBuiltins "||" exprs = Just <$> operateOnExprs (ValBool . any valToBool) exprs
--
evaluateBuiltins _ _ = pure Nothing

numberBinaryOp :: (Double -> Double -> EvalValue) -> Expr -> Expr -> Evaluator m EvalValue
numberBinaryOp fn exp1 exp2 = do
  v1 <- valToNumber <$> interpretExpression exp1
  v2 <- valToNumber <$> interpretExpression exp2
  pure $ fn v1 v2

operateOnExprs :: ([EvalValue] -> EvalValue) -> [Expr] -> Evaluator m EvalValue
operateOnExprs fn exprs = fn <$> mapM interpretExpression exprs

evaluateCall :: EvalValue -> [Expr] -> Evaluator m EvalValue
evaluateCall (ValLambda labels body) argsE = do
  argsScope <- argsToScope labels argsE interpretExpression
  withSandboxedScope $ do
    modify' $ insertNewScope argsScope
    interpretExpression body
evaluateCall (ValMacro labels body) argsE = do
  argsScope <- argsToScope labels argsE (pure . ValQuoted)
  result <- withSandboxedScope $ do
    modify' $ insertNewScope argsScope
    interpretExpression body
  case result of
    ValQuoted expr -> interpretExpression expr
    _ -> pure result
evaluateCall value _args = pure value

interpretAll :: [Expr] -> Evaluator m [EvalValue]
interpretAll = mapM interpretExpression

runExprEvaluatorWithCallStack :: (MonadIO m) => ExceptT EvalError (StateT EvalEnvironment m) a -> EvalEnvironment -> m (Either EvalError a, EvalEnvironment)
runExprEvaluatorWithCallStack = runStateT . runExceptT

runInterpreterRaw :: (MonadIO m) => EvalEnvironment -> [Expr] -> m (Either EvalError [EvalValue], EvalEnvironment)
runInterpreterRaw env = (`runExprEvaluatorWithCallStack` env) . interpretAll

runExprInterpreterRaw :: (MonadIO m) => EvalEnvironment -> Expr -> m (Either EvalError EvalValue, EvalEnvironment)
runExprInterpreterRaw env = (`runExprEvaluatorWithCallStack` env) . interpretExpression
