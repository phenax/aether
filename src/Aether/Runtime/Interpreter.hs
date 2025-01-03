module Aether.Runtime.Interpreter where

import Aether.Types
import Control.Monad.Except (ExceptT, MonadError (throwError), runExceptT)
import Control.Monad.State (MonadIO, MonadState (get, put), StateT (runStateT), gets, modify')
import qualified Data.Map as Map

type Scope = Map.Map String EvalValue

newtype EvalEnvironment = EvalEnvironment {envCallStack :: [Scope]}
  deriving (Show, Eq)

instance Semigroup EvalEnvironment where
  (<>) a b = EvalEnvironment {envCallStack = envCallStack a <> envCallStack b}

instance Monoid EvalEnvironment where
  mempty = EvalEnvironment {envCallStack = [Map.empty]}

defineInCurrentScope :: String -> EvalValue -> EvalEnvironment -> EvalEnvironment
defineInCurrentScope name value env@(EvalEnvironment {envCallStack = (headStack : rest)}) =
  env {envCallStack = Map.insert name value headStack : rest}
defineInCurrentScope _ _ env = env

insertNewScope :: Scope -> EvalEnvironment -> EvalEnvironment
insertNewScope calls env = env {envCallStack = calls : envCallStack env}

type Evaluator m a = (MonadState EvalEnvironment m, MonadError EvalError m) => m a

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

interpretLiteral :: Literal -> Evaluator m EvalValue
interpretLiteral (LitString str) = pure $ ValString str
interpretLiteral (LitBool bool) = pure $ ValBool bool
interpretLiteral (LitNumber num) = pure $ ValNumber num
interpretLiteral LitNil = pure ValNil

argToLabel :: Expr -> String
argToLabel (ExprSymbol sym) = sym
argToLabel _ = "_"

valToNumber :: EvalValue -> Double
valToNumber (ValNumber num) = num
valToNumber (ValString num) = read num
valToNumber (ValBool True) = 1
valToNumber (ValQuoted (ExprLiteral (LitNumber n))) = n
valToNumber (ValQuoted (ExprLiteral (LitString str))) = read str
valToNumber _ = 0

interpretExpression :: Expr -> Evaluator m EvalValue
interpretExpression (ExprLiteral lit) = interpretLiteral lit
interpretExpression (ExprQuoted (ExprSymList [])) = pure ValNil
interpretExpression (ExprQuoted quote) = pure $ ValQuoted quote
interpretExpression (ExprSymbol sym) = gets (lookupSymbol sym) >>= maybe (throwError $ NameNotFound sym) pure
interpretExpression (ExprSymList []) = pure ValNil
interpretExpression (ExprSymList (ExprSymbol name : argsE)) =
  evaluateBuiltins name argsE >>= maybe (interpretExpression (ExprSymbol name) >>= (`evaluateCall` argsE)) pure
interpretExpression (ExprSymList (fnE : argsE)) = interpretExpression fnE >>= (`evaluateCall` argsE)

evaluateBuiltins :: String -> [Expr] -> Evaluator m (Maybe EvalValue)
evaluateBuiltins "set" [ExprSymbol name, valueE] = do
  value <- interpretExpression valueE
  modify' $ defineInCurrentScope name value
  pure $ Just value
evaluateBuiltins "set" _ = throwError $ TypeError "Invalid call to set"
evaluateBuiltins "->" (ExprSymList argsE : body) = do
  let lambda = ValLambda (argToLabel <$> argsE) $ ExprSymList (ExprSymbol "do" : body)
  pure $ Just lambda
evaluateBuiltins "defmacro" (ExprSymList (ExprSymbol name : argsE) : body) = do
  let macro = ValMacro (argToLabel <$> argsE) $ ExprSymList (ExprSymbol "do" : body)
  modify' $ defineInCurrentScope name macro
  pure $ Just ValNil
evaluateBuiltins "defmacro" _ = throwError $ TypeError "Invalid call to defmacro"
evaluateBuiltins "define" (ExprSymList (ExprSymbol name : argsE) : body) = do
  let value = ValLambda (argToLabel <$> argsE) $ ExprSymList (ExprSymbol "do" : body)
  modify' $ defineInCurrentScope name value
  pure $ Just ValNil
evaluateBuiltins "define" _ = throwError $ TypeError "Invalid call to define"
evaluateBuiltins "do" body = do
  results <- withSandboxedScope $ do
    modify' $ insertNewScope Map.empty
    mapM interpretExpression body
  pure $ Just $ case results of
    [] -> ValNil
    _ -> last results
evaluateBuiltins "eval" (expr : _) = do
  res <- interpretExpression expr
  case res of
    ValQuoted quote -> Just <$> interpretExpression quote
    _ -> pure . Just $ res
evaluateBuiltins "eval" _ = throwError $ TypeError "Invalid number of arguments sent to eval"
evaluateBuiltins "+" exprs = do
  values <- mapM interpretExpression exprs
  pure . Just . ValNumber $ sumVals values 0
  where
    sumVals [] num = num
    sumVals (arg : args) num = sumVals args $ num + valToNumber arg
evaluateBuiltins "-" exprs = do
  values <- mapM interpretExpression exprs
  pure . Just . ValNumber $ case values of
    [] -> 0
    [x] -> -valToNumber x
    _ -> diffVals values 0
  where
    diffVals [] num = num
    diffVals (arg : args) num = diffVals args $ valToNumber arg - num
evaluateBuiltins _ _ = pure Nothing

argsToScope :: [String] -> [Expr] -> (Expr -> Evaluator m EvalValue) -> Evaluator m Scope
argsToScope labels argsE expToVal = do
  args <- mapM expToVal argsE
  pure $ Map.fromList $ labels `zip` args

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
evaluateCall value _args = pure value -- TODO: impl

interpretAll :: [Expr] -> Evaluator m [EvalValue]
interpretAll = mapM interpretExpression

runExprEvaluatorWithCallStack :: (MonadIO m) => ExceptT EvalError (StateT EvalEnvironment m) a -> EvalEnvironment -> m (Either EvalError a, EvalEnvironment)
runExprEvaluatorWithCallStack = runStateT . runExceptT

runExprInterpreter :: (MonadIO m) => Expr -> m (Either EvalError EvalValue)
runExprInterpreter = fmap fst . (`runExprEvaluatorWithCallStack` mempty) . interpretExpression

runInterpreter :: (MonadIO m) => [Expr] -> m (Either EvalError [EvalValue])
runInterpreter = fmap fst . (`runExprEvaluatorWithCallStack` mempty) . interpretAll