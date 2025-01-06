module Aether.Runtime.Interpreter where

import Aether.Runtime.Scope (argsToScope, closure, defineInCurrentScope, lookupSymbol, mkScope)
import Aether.Runtime.Value
import Aether.Types
import Control.Monad.Except (ExceptT, MonadError (throwError), runExceptT)
import Control.Monad.State.Strict (MonadIO, StateT (runStateT), gets, modify')
import qualified Data.Map.Strict as Map

interpretLiteral :: Literal -> Evaluator m EvalValue
interpretLiteral = \case
  LitString str -> pure $ ValString str
  LitBool bool -> pure $ ValBool bool
  LitNumber num -> pure $ ValNumber num
  LitNil -> pure ValNil

interpretExpression :: Expr -> Evaluator m EvalValue
interpretExpression = \case
  ExprLiteral lit -> interpretLiteral lit
  ExprSymbol sym -> do
    -- Debug.traceShowM sym
    gets (lookupSymbol sym) >>= maybe (throwError $ NameNotFound sym) pure
  ExprSymList [] -> pure ValNil
  ExprSymList (ExprSymbol name : argsE) -> do
    -- Debug.traceShowM ("call", name)
    !builtinResult <- evaluateBuiltins name argsE
    case builtinResult of
      Just result -> pure result
      Nothing -> interpretExpression (ExprSymbol name) >>= (`evaluateCall` argsE)
  ExprSymList (fnE : argsE) -> interpretExpression fnE >>= (`evaluateCall` argsE)
  ExprUnquoted _expr -> undefined -- TODO: either throw error or `interpretExpression expr`
  ExprSpliced _expr -> undefined -- same interpretExpression expr
  -- ExprValue (ValQuoted expr) -> interpretExpression expr
  ExprValue value -> pure value
  ExprQuoted (ExprSymList []) -> pure ValNil
  ExprQuoted quote -> ValQuoted <$> evalUnquotes quote
    where
      evalUnquotes :: Expr -> Evaluator m Expr
      evalUnquotes (ExprUnquoted expr) = ExprValue <$> interpretExpression expr
      evalUnquotes (ExprSymList exprs) = ExprSymList <$> (mapM evalUnquotes exprs >>= evalSplices)
      evalUnquotes expr = pure expr

      evalSplices :: [Expr] -> Evaluator m [Expr]
      evalSplices [] = pure []
      evalSplices (ExprSpliced expr : exprs) = do
        val <- interpretExpression expr
        spliced <- evalSplices exprs
        (++ spliced) <$> expandSymList val
      evalSplices (expr : exprs) = (expr :) <$> evalSplices exprs

      expandSymList :: EvalValue -> Evaluator m [Expr]
      expandSymList (ValQuoted (ExprSymList exprs)) = pure exprs
      expandSymList (ValQuoted (ExprQuoted (ExprSymList exprs))) = pure exprs
      expandSymList (ValQuoted sym@(ExprSymbol _)) = interpretExpression sym >>= expandSymList
      expandSymList e = pure [ExprValue e]

evaluateBuiltins :: String -> [Expr] -> Evaluator m (Maybe EvalValue)
-- Set a value in current scope
evaluateBuiltins "set" [ExprSymbol name, valueE] = do
  !value <- interpretExpression valueE
  modify' $ defineInCurrentScope name value
  pure $ Just ValNil
evaluateBuiltins "set" args = do throwError $ TypeError $ "Invalid call to set: " ++ show args

-- Lambda expression syntax
evaluateBuiltins "->" (ExprSymList argsE : bodyE) = do
  stack <- gets envCallStack
  pure . Just $ ValLambda stack argLabels body
  where
    argLabels = argToLabel <$> argsE
    body = if length bodyE == 1 then head bodyE else ExprSymList (ExprSymbol "do" : bodyE)
evaluateBuiltins "->" _ = do throwError $ TypeError "Invalid call to ->"

-- Define lambda in current scope
evaluateBuiltins "define" (ExprSymList (ExprSymbol name : argsE) : bodyE) = do
  stack <- gets envCallStack
  modify' $ defineInCurrentScope name (ValLambda stack argLabels body)
  pure $ Just ValNil
  where
    argLabels = argToLabel <$> argsE
    body = if length bodyE == 1 then head bodyE else ExprSymList (ExprSymbol "do" : bodyE)
evaluateBuiltins "define" _ = do throwError $ TypeError "Invalid call to define"

-- Define macro in current scope
evaluateBuiltins "defmacro" (ExprSymList (ExprSymbol name : argsE) : bodyE) = do
  stack <- gets envCallStack
  modify' (defineInCurrentScope name $ ValMacro stack argLabels body)
  pure $ Just ValNil
  where
    argLabels = argToLabel <$> argsE
    body = if length bodyE == 1 then head bodyE else ExprSymList (ExprSymbol "do" : bodyE)
evaluateBuiltins "defmacro" _ = do throwError $ TypeError "Invalid call to defmacro"

-- Do syntax (chained expressions)
evaluateBuiltins "do" body = do
  (Stack stack) <- gets envCallStack
  !scope <- mkScope Map.empty
  !results <- closure (Stack $ scope : stack) $ do
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

-- Get first element from quoted symlist
evaluateBuiltins "car" [expr] = do
  res <- interpretExpression expr
  case res of
    ValQuoted (ExprSymList (first : _)) -> Just <$> interpretExpression first
    ValQuoted (ExprSymList []) -> pure $ Just ValNil
    ValQuoted quote -> Just <$> interpretExpression quote
    _ -> pure . Just $ res
evaluateBuiltins "car" _ = do throwError $ TypeError "Invalid number of arguments sent to car"

-- Get tail elements from quoted symlist
evaluateBuiltins "cdr" [expr] = do
  res <- interpretExpression expr
  case res of
    ValQuoted (ExprSymList [_]) -> pure $ Just ValNil
    ValQuoted (ExprSymList (_ : rest)) ->
      Just . ValQuoted . ExprSymList . fmap ExprValue <$> mapM interpretExpression rest
    _ -> pure $ Just ValNil
evaluateBuiltins "cdr" _ = do throwError $ TypeError "Invalid number of arguments sent to cdr"

-- Construct pair
evaluateBuiltins "cons" [itemE, restE] = do
  item <- interpretExpression itemE
  rest <- interpretExpression restE
  case rest of
    ValQuoted (ExprSymList values) -> pure . Just $ ValQuoted (ExprSymList (ExprValue item : values))
    ValNil -> pure . Just $ ValQuoted (ExprSymList [ExprValue item])
    ValQuoted (ExprLiteral LitNil) -> pure . Just $ ValQuoted (ExprSymList [ExprValue item])
    ValQuoted (ExprValue ValNil) -> pure . Just $ ValQuoted (ExprSymList [ExprValue item])
    _ -> pure . Just $ ValQuoted (ExprSymList [ExprValue item, ExprValue rest])
evaluateBuiltins "cons" _ = do throwError $ TypeError "Invalid number of arguments sent to cons"

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
evaluateBuiltins "&&" exprs = Just <$> operateOnExprs (ValBool . all valToBool) exprs
evaluateBuiltins "||" exprs = Just <$> operateOnExprs (ValBool . any valToBool) exprs
-- Type helpers
evaluateBuiltins "type" [expr] = Just . ValQuoted . ExprSymbol . typeOfValue <$> interpretExpression expr
evaluateBuiltins "lt?" _ = throwError $ TypeError "Invalid number of arguments for lt?"
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
-- Lambda
evaluateCall (ValLambda (Stack stack) labels body) argsE = do
  args <- mapM interpretExpression argsE
  argsScope <- argsToScope labels args
  closure (Stack $ argsScope : stack) $ interpretExpression body

-- Macros
-- TODO: Figure out how to lexically scope macros
evaluateCall (ValMacro (Stack _defnstack) labels body) argsE = do
  (Stack callerstack) <- gets envCallStack
  !args <- mapM interpretMacroArgs argsE
  !argsScope <- argsToScope labels args
  !result <- closure (Stack $ argsScope : callerstack) $ interpretExpression body
  case result of
    ValQuoted expr -> interpretExpression $ unwrapQuotes expr
    _ -> pure result
  where
    interpretMacroArgs :: Expr -> Evaluator m EvalValue
    interpretMacroArgs (ExprLiteral lit) = interpretLiteral lit
    interpretMacroArgs (ExprValue v) = pure v
    interpretMacroArgs (ExprQuoted quote) = pure $ ValQuoted quote
    interpretMacroArgs (ExprSymList exprs) = ValQuoted . ExprSymList . fmap ExprValue <$> mapM interpretMacroArgs exprs
    interpretMacroArgs val = pure $ ValQuoted val

    unwrapQuotes :: Expr -> Expr
    unwrapQuotes (ExprSymList exprs) = ExprSymList $ map unwrapQuotes exprs
    unwrapQuotes (ExprValue (ValQuoted (ExprSymList exprs))) = ExprSymList $ map unwrapQuotes exprs
    unwrapQuotes (ExprValue (ValQuoted e)) = e
    unwrapQuotes e = e

-- Quoted values
evaluateCall (ValQuoted quote) argsE = do
  !fn <- interpretExpression quote
  evaluateCall fn argsE

-- Booleans are callable (Kite/Kestral)
evaluateCall (ValBool bool) argsE = do
  case argsE of
    (then_ : _) | bool -> interpretExpression then_
    (_ : else_ : _) | not bool -> interpretExpression else_
    _ -> pure ValNil

-- Invald call
evaluateCall value _args = do throwError $ TypeError $ "Can't call value: " ++ show value -- ++ " with: " ++ show args

interpretAll :: [Expr] -> Evaluator m [EvalValue]
interpretAll = mapM interpretExpression

runExprEvaluatorWithCallStack :: (MonadIO m) => ExceptT EvalError (StateT EvalEnvironment m) a -> EvalEnvironment -> m (Either EvalError a, EvalEnvironment)
runExprEvaluatorWithCallStack = runStateT . runExceptT

runInterpreterRaw :: (MonadIO m) => EvalEnvironment -> [Expr] -> m (Either EvalError [EvalValue], EvalEnvironment)
runInterpreterRaw env = (`runExprEvaluatorWithCallStack` env) . interpretAll

runExprInterpreterRaw :: (MonadIO m) => EvalEnvironment -> Expr -> m (Either EvalError EvalValue, EvalEnvironment)
runExprInterpreterRaw env = (`runExprEvaluatorWithCallStack` env) . interpretExpression
