module Aether.Runtime.Interpreter where

import Aether.Runtime.Scope (argsToScope, closure, defineInCurrentScope, lookupSymbol, mkScope)
import Aether.Runtime.Value
import Aether.Types
import Control.Monad ((>=>))
import Control.Monad.Except (ExceptT, MonadError (throwError), runExceptT)
import Control.Monad.State.Strict (MonadIO, MonadState, StateT (runStateT), gets, modify')
import qualified Data.Map.Strict as Map

subtractVal :: (Num a) => [a] -> a
subtractVal [] = 0
subtractVal [x] = -x
subtractVal (x : xs) = x - sum xs

divideVal :: (Fractional a) => [a] -> a
divideVal [] = 1
divideVal (x : xs) = x / product xs

builtins :: (MonadState EvalEnvironment m, MonadError EvalError m, MonadLangIO m) => m (Map.Map Name ([Expr] -> m EvalValue))
builtins =
  pure $
    Map.fromList
      [ ("set", builtinSetE),
        ("->", builtinLambdaE),
        ("define", builtinDefineE),
        ("defmacro", builtinDefmacroE),
        ("do", builtinDoE),
        ("progn", builtinPrognE),
        ("eval", builtinEvalE),
        ("car", builtinCarE),
        ("cdr", builtinCdrE),
        ("cons", builtinConsE),
        ("+", operateOnExprs (ValNumber . sum . fmap valToNumber)),
        ("*", operateOnExprs (ValNumber . product . fmap valToNumber)),
        ("-", operateOnExprs (ValNumber . subtractVal . fmap valToNumber)),
        ("/", operateOnExprs (ValNumber . divideVal . fmap valToNumber)),
        ("lt?", builtinLessThan),
        ("gt?", builtinGreaterThan),
        ("lte?", builtinLessThanOrEqualTo),
        ("gte?", builtinGreaterThanOrEqualTo),
        ("eq?", operateOnExprs (ValBool . checkIfAllEqual)),
        ("&&", operateOnExprs (ValBool . all valToBool)),
        ("||", operateOnExprs (ValBool . any valToBool)),
        ("type", builtinTypeOf),
        ("display", builtinDisplay),
        ("displayNl", builtinDisplay . (++ [ExprLiteral $ LitString "\n"]))
      ]

builtinDisplay :: [Expr] -> Evaluator m EvalValue
builtinDisplay exprs = do
  let displayVal = putStringToScreen . showEvalValue
  mapM_ (interpretExpression >=> displayVal) exprs
  pure ValNil

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
    isBuiltin <- Map.member sym <$> builtins
    let fallback
          | isBuiltin = pure $ ValBuiltin sym
          | otherwise = throwError $ NameNotFound sym
    gets (lookupSymbol sym) >>= maybe fallback pure
  ExprSymList [] -> pure ValNil
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

builtinSetE :: [Expr] -> Evaluator m EvalValue
builtinSetE [ExprSymbol name, valueE] = do
  !value <- interpretExpression valueE
  modify' $ defineInCurrentScope name value
  pure ValNil
builtinSetE args = do throwError $ TypeError $ "Invalid call to set: " ++ show args

builtinLambdaE :: [Expr] -> Evaluator m EvalValue
builtinLambdaE (ExprSymList argsE : bodyE) = do
  stack <- gets envCallStack
  pure $ ValLambda stack argLabels body
  where
    argLabels = argToLabel <$> argsE
    body = if length bodyE == 1 then head bodyE else ExprSymList (ExprSymbol "progn" : bodyE)
builtinLambdaE _ = do throwError $ TypeError "Invalid call to ->"

builtinDefineE :: [Expr] -> Evaluator m EvalValue
builtinDefineE (ExprSymList (ExprSymbol name : argsE) : bodyE) = do
  stack <- gets envCallStack
  modify' $ defineInCurrentScope name (ValLambda stack argLabels body)
  pure ValNil
  where
    argLabels = argToLabel <$> argsE
    body = if length bodyE == 1 then head bodyE else ExprSymList (ExprSymbol "progn" : bodyE)
builtinDefineE _ = do throwError $ TypeError "Invalid call to define"

builtinDefmacroE :: [Expr] -> Evaluator m EvalValue
builtinDefmacroE (ExprSymList (ExprSymbol name : argsE) : bodyE) = do
  stack <- gets envCallStack
  modify' (defineInCurrentScope name $ ValMacro stack argLabels body)
  pure ValNil
  where
    argLabels = argToLabel <$> argsE
    body = if length bodyE == 1 then head bodyE else ExprSymList (ExprSymbol "progn" : bodyE)
builtinDefmacroE _ = do throwError $ TypeError "Invalid call to defmacro"

builtinDoE :: [Expr] -> Evaluator m EvalValue
builtinDoE body = do
  (Stack stack) <- gets envCallStack
  !scope <- mkScope Map.empty
  !results <- closure (Stack $ scope : stack) $ do
    mapM interpretExpression body
  pure $ case results of
    [] -> ValNil
    _ -> last results

builtinPrognE :: [Expr] -> Evaluator m EvalValue
builtinPrognE body = do
  results <- mapM interpretExpression body
  pure $ case results of
    [] -> ValNil
    _ -> last results

builtinEvalE :: [Expr] -> Evaluator m EvalValue
builtinEvalE [expr] = do
  res <- interpretExpression expr
  case res of
    ValQuoted quote -> interpretExpression quote
    _ -> pure res
builtinEvalE _ = do throwError $ TypeError "Invalid number of arguments sent to eval"

builtinCarE :: [Expr] -> Evaluator m EvalValue
builtinCarE [expr] = do
  res <- interpretExpression expr
  case res of
    ValQuoted (ExprSymList (first : _)) -> interpretExpression first
    ValQuoted (ExprSymList []) -> pure ValNil
    ValQuoted quote -> interpretExpression quote
    _ -> pure res
builtinCarE _ = do throwError $ TypeError "Invalid number of arguments sent to car"

builtinCdrE :: [Expr] -> Evaluator m EvalValue
builtinCdrE [expr] = do
  res <- interpretExpression expr
  case res of
    ValQuoted (ExprSymList [_]) -> pure ValNil
    ValQuoted (ExprSymList (_ : rest)) ->
      ValQuoted . ExprSymList . fmap ExprValue <$> mapM interpretExpression rest
    _ -> pure ValNil
builtinCdrE _ = do throwError $ TypeError "Invalid number of arguments sent to cdr"

builtinConsE :: [Expr] -> Evaluator m EvalValue
builtinConsE [itemE, restE] = do
  item <- interpretExpression itemE
  rest <- interpretExpression restE
  case rest of
    ValQuoted (ExprSymList values) -> pure $ ValQuoted (ExprSymList (ExprValue item : values))
    ValNil -> pure $ ValQuoted (ExprSymList [ExprValue item])
    ValQuoted (ExprLiteral LitNil) -> pure $ ValQuoted (ExprSymList [ExprValue item])
    ValQuoted (ExprValue ValNil) -> pure $ ValQuoted (ExprSymList [ExprValue item])
    _ -> pure $ ValQuoted (ExprSymList [ExprValue item, ExprValue rest])
builtinConsE _ = do throwError $ TypeError "Invalid number of arguments sent to cons"

builtinLessThan :: [Expr] -> Evaluator m EvalValue
builtinLessThan [exp1, exp2] = numberBinaryOp (\a -> ValBool . (a <)) exp1 exp2
builtinLessThan _ = throwError $ TypeError "Invalid number of arguments for lt?"

builtinGreaterThan :: [Expr] -> Evaluator m EvalValue
builtinGreaterThan [exp1, exp2] = numberBinaryOp (\a -> ValBool . (a >)) exp1 exp2
builtinGreaterThan _ = throwError $ TypeError "Invalid number of arguments for gt?"

builtinLessThanOrEqualTo :: [Expr] -> Evaluator m EvalValue
builtinLessThanOrEqualTo [exp1, exp2] = numberBinaryOp (\a -> ValBool . (a <=)) exp1 exp2
builtinLessThanOrEqualTo _ = throwError $ TypeError "Invalid number of arguments for lte?"

builtinGreaterThanOrEqualTo :: [Expr] -> Evaluator m EvalValue
builtinGreaterThanOrEqualTo [exp1, exp2] = numberBinaryOp (\a -> ValBool . (a >=)) exp1 exp2
builtinGreaterThanOrEqualTo _ = throwError $ TypeError "Invalid number of arguments for gte?"

builtinTypeOf :: [Expr] -> Evaluator m EvalValue
builtinTypeOf [expr] = ValQuoted . ExprSymbol . typeOfValue <$> interpretExpression expr
builtinTypeOf _ = throwError $ TypeError "Invalid number of arguments for type"

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

-- Primitive evaluation
evaluateCall (ValBuiltin name) argsE = do
  builtin <- Map.lookup name <$> builtins
  case builtin of
    Just eval -> eval argsE
    Nothing -> throwError $ UnknownError "Primtive not defined"

-- Quoted values
evaluateCall (ValQuoted quote) argsE = do
  !fn <- interpretExpression quote
  evaluateCall fn argsE

-- Booleans are callable
evaluateCall (ValBool bool) argsE = do
  case argsE of
    (then_ : _) | bool -> interpretExpression then_
    (_ : else_ : _) | not bool -> interpretExpression else_
    _ -> pure ValNil

-- Invald call
evaluateCall value _args = do throwError $ TypeError $ "Can't call value: " ++ show value -- ++ " with: " ++ show args

--
runExprEvaluatorWithCallStack ::
  (MonadIO m) =>
  LangIOT (ExceptT EvalError (StateT EvalEnvironment m)) a ->
  EvalEnvironment ->
  m (Either EvalError a, EvalEnvironment)
runExprEvaluatorWithCallStack m = runStateT (runExceptT $ runLangIOT m)

runExprInterpreterRaw :: (MonadIO m) => EvalEnvironment -> Expr -> m (Either EvalError EvalValue, EvalEnvironment)
runExprInterpreterRaw env = (`runExprEvaluatorWithCallStack` env) . interpretExpression

runInterpreterRaw :: (MonadIO m) => EvalEnvironment -> [Expr] -> m (Either EvalError [EvalValue], EvalEnvironment)
runInterpreterRaw env = (`runExprEvaluatorWithCallStack` env) . mapM interpretExpression
