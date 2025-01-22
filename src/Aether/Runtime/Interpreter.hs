module Aether.Runtime.Interpreter where

import Aether.Runtime.Scope (argsToScope, closure, defineInCurrentScope, lookupSymbol, mkScope)
import Aether.Runtime.Value
import Aether.Types
import Control.Monad ((>=>))
import Control.Monad.Except (ExceptT, MonadError (catchError, throwError), runExceptT)
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
        ("quote", builtinQuote),
        ("try", builtinTry),
        ("error!", builtinError),
        ("displayNl", builtinDisplay . (++ [ExprLiteral NullSpan $ LitString "\n"]))
      ]

interpretLiteral :: Literal -> Evaluator m EvalValue
interpretLiteral = \case
  LitString str -> pure $ ValString str
  LitBool bool -> pure $ ValBool bool
  LitNumber num -> pure $ ValNumber num
  LitNil -> pure ValNil

interpretExpression :: Expr -> Evaluator m EvalValue
interpretExpression = \case
  ExprLiteral _ lit -> interpretLiteral lit
  ExprSymbol _ sym -> do
    isBuiltin <- Map.member sym <$> builtins
    let fallback
          | isBuiltin = pure $ ValBuiltin sym
          | otherwise = throwError $ NameNotFound sym
    gets (lookupSymbol sym) >>= maybe fallback pure
  ExprSymList _ [] -> pure ValNil
  ExprSymList _ (fnE : argsE) -> interpretExpression fnE >>= (`evaluateCall` argsE)
  ExprUnquoted _ _expr -> undefined -- TODO: either throw error or `interpretExpression expr`
  ExprSpliced _ _expr -> undefined -- same interpretExpression expr
  -- ExprValue (ValQuoted expr) -> interpretExpression expr
  ExprValue value -> pure value
  ExprQuoted _ (ExprSymList _ []) -> pure ValNil
  ExprQuoted _ quote -> ValQuoted <$> evalUnquotes quote
    where
      evalUnquotes :: Expr -> Evaluator m Expr
      evalUnquotes (ExprUnquoted _ expr) = ExprValue <$> interpretExpression expr
      evalUnquotes (ExprSymList _ exprs) = ExprSymList NullSpan <$> (mapM evalUnquotes exprs >>= evalSplices)
      evalUnquotes expr = pure expr

      evalSplices :: [Expr] -> Evaluator m [Expr]
      evalSplices [] = pure []
      evalSplices (ExprSpliced _ expr : exprs) = do
        val <- interpretExpression expr
        spliced <- evalSplices exprs
        (++ spliced) <$> expandSymList val
      evalSplices (expr : exprs) = (expr :) <$> evalSplices exprs

      expandSymList :: EvalValue -> Evaluator m [Expr]
      expandSymList (ValQuoted (ExprSymList _ exprs)) = pure exprs
      expandSymList (ValQuoted (ExprQuoted _ (ExprSymList _ exprs))) = pure exprs
      expandSymList (ValQuoted sym@(ExprSymbol _ _)) = interpretExpression sym >>= expandSymList
      expandSymList e = pure [ExprValue e]

builtinQuote :: [Expr] -> Evaluator m EvalValue
builtinQuote [expr] = pure $ ValQuoted expr
builtinQuote args = throwError $ ArgumentLengthError True 1 (length args) "quote"

builtinDisplay :: [Expr] -> Evaluator m EvalValue
builtinDisplay exprs = do
  let displayVal = putStringToScreen . showEvalValue
  mapM_ (interpretExpression >=> displayVal) exprs
  pure ValNil

builtinSetE :: [Expr] -> Evaluator m EvalValue
builtinSetE [ExprSymbol _ name, valueE] = do
  !value <- interpretExpression valueE
  modify' $ defineInCurrentScope name value
  pure ValNil
builtinSetE args = do throwError $ TypeError $ "Invalid call to set: " ++ show args

builtinDefineE :: [Expr] -> Evaluator m EvalValue
builtinDefineE (ExprSymList sourceSpan (ExprSymbol _ name : argsE) : bodyE) = do
  lambda <- createLambda sourceSpan argsE bodyE
  modify' $ defineInCurrentScope name lambda
  pure ValNil
builtinDefineE [ExprSymbol _ name, valueE] = do
  value <- interpretExpression valueE
  modify' $ defineInCurrentScope name value
  pure ValNil
builtinDefineE (ExprSymbol _ _ : args) =
  do throwError $ ArgumentLengthError True 1 (length args) "define"
builtinDefineE _ = do throwError $ TypeError "Invalid call to define"

builtinLambdaE :: [Expr] -> Evaluator m EvalValue
builtinLambdaE (ExprSymList sourceSpan argsE : bodyE) = createLambda sourceSpan argsE bodyE
builtinLambdaE _ = do throwError $ TypeError "Invalid call to ->"

createLambda :: SourceSpan -> [Expr] -> [Expr] -> Evaluator m EvalValue
createLambda sourceSpan argsE bodyE = do
  stack <- gets envCallStack
  pure $ ValLambda stack sourceSpan argLabels body
  where
    argLabels = argToLabel <$> argsE
    body = if length bodyE == 1 then head bodyE else ExprSymList NullSpan (ExprSymbol NullSpan "progn" : bodyE)

builtinDefmacroE :: [Expr] -> Evaluator m EvalValue
builtinDefmacroE (ExprSymList sourceSpan (ExprSymbol _ name : argsE) : bodyE) = do
  stack <- gets envCallStack
  modify' (defineInCurrentScope name $ ValMacro stack sourceSpan argLabels body)
  pure ValNil
  where
    argLabels = argToLabel <$> argsE
    body = if length bodyE == 1 then head bodyE else ExprSymList NullSpan (ExprSymbol NullSpan "progn" : bodyE)
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
builtinEvalE exprs = do throwError $ ArgumentLengthError True 1 (length exprs) "eval"

builtinCarE :: [Expr] -> Evaluator m EvalValue
builtinCarE [expr] = do
  res <- interpretExpression expr
  case res of
    ValQuoted (ExprSymList _ (first : _)) -> interpretExpression first
    ValQuoted (ExprSymList _ []) -> pure ValNil
    ValQuoted quote -> interpretExpression quote
    _ -> pure res
builtinCarE exprs = do throwError $ ArgumentLengthError True 1 (length exprs) "car"

builtinCdrE :: [Expr] -> Evaluator m EvalValue
builtinCdrE [expr] = do
  res <- interpretExpression expr
  case res of
    ValQuoted (ExprSymList _ [_]) -> pure ValNil
    ValQuoted (ExprSymList _ (_ : rest)) ->
      ValQuoted . ExprSymList NullSpan . fmap ExprValue <$> mapM interpretExpression rest
    _ -> pure ValNil
builtinCdrE exprs = do throwError $ ArgumentLengthError True 1 (length exprs) "cdr"

builtinConsE :: [Expr] -> Evaluator m EvalValue
builtinConsE [itemE, restE] = do
  item <- interpretExpression itemE
  rest <- interpretExpression restE
  case rest of
    ValQuoted (ExprSymList _ values) -> pure $ ValQuoted (ExprSymList NullSpan (ExprValue item : values))
    ValNil -> pure $ ValQuoted (ExprSymList NullSpan [ExprValue item])
    ValQuoted (ExprLiteral _ LitNil) -> pure $ ValQuoted (ExprSymList NullSpan [ExprValue item])
    ValQuoted (ExprValue ValNil) -> pure $ ValQuoted (ExprSymList NullSpan [ExprValue item])
    _ -> pure $ ValQuoted (ExprSymList NullSpan [ExprValue item, ExprValue rest])
builtinConsE exprs = do throwError $ ArgumentLengthError True 2 (length exprs) "cons"

builtinLessThan :: [Expr] -> Evaluator m EvalValue
builtinLessThan [exp1, exp2] = numberBinaryOp (\a -> ValBool . (a <)) exp1 exp2
builtinLessThan exprs = throwError $ ArgumentLengthError True 2 (length exprs) "lt?"

builtinGreaterThan :: [Expr] -> Evaluator m EvalValue
builtinGreaterThan [exp1, exp2] = numberBinaryOp (\a -> ValBool . (a >)) exp1 exp2
builtinGreaterThan exprs = throwError $ ArgumentLengthError True 2 (length exprs) "gt?"

builtinLessThanOrEqualTo :: [Expr] -> Evaluator m EvalValue
builtinLessThanOrEqualTo [exp1, exp2] = numberBinaryOp (\a -> ValBool . (a <=)) exp1 exp2
builtinLessThanOrEqualTo exprs = throwError $ ArgumentLengthError True 2 (length exprs) "lte?"

builtinGreaterThanOrEqualTo :: [Expr] -> Evaluator m EvalValue
builtinGreaterThanOrEqualTo [exp1, exp2] = numberBinaryOp (\a -> ValBool . (a >=)) exp1 exp2
builtinGreaterThanOrEqualTo exprs = throwError $ ArgumentLengthError True 2 (length exprs) "gte?"

builtinTypeOf :: [Expr] -> Evaluator m EvalValue
builtinTypeOf [expr] = ValQuoted . ExprSymbol NullSpan . typeOfValue <$> interpretExpression expr
builtinTypeOf exprs = throwError $ ArgumentLengthError True 1 (length exprs) "type"

builtinTry :: [Expr] -> Evaluator m EvalValue
builtinTry exprs = catchError (toRight <$> tryValue) (pure . toLeft . evalErrorToValue)
  where
    tryValue = interpretExpression (ExprSymList NullSpan $ ExprSymbol NullSpan "do" : exprs)
    toRight v = ValQuoted $ ExprSymList NullSpan [ExprValue ValNil, ExprValue v]
    toLeft e = ValQuoted $ ExprSymList NullSpan [ExprValue e, ExprValue ValNil]

builtinError :: [Expr] -> Evaluator m EvalValue
builtinError (labelE : messageE : _) = do
  label <- interpretExpression labelE
  message <- interpretExpression messageE
  throwError $ UserError label message
builtinError [labelE] = do
  label <- interpretExpression labelE
  throwError $ UserError label ValNil
builtinError [] = throwError $ UserError (ValQuoted $ ExprSymbol NullSpan "error") ValNil

numberBinaryOp :: (Double -> Double -> EvalValue) -> Expr -> Expr -> Evaluator m EvalValue
numberBinaryOp fn exp1 exp2 = do
  v1 <- valToNumber <$> interpretExpression exp1
  v2 <- valToNumber <$> interpretExpression exp2
  pure $ fn v1 v2

operateOnExprs :: ([EvalValue] -> EvalValue) -> [Expr] -> Evaluator m EvalValue
operateOnExprs fn exprs = fn <$> mapM interpretExpression exprs

evaluateCall :: EvalValue -> [Expr] -> Evaluator m EvalValue
-- Lambda
evaluateCall (ValLambda (Stack stack) _ labels body) argsE = do
  args <- mapM interpretExpression argsE
  argsScope <- argsToScope labels args
  closure (Stack $ argsScope : stack) $ interpretExpression body

-- Macros
-- TODO: Figure out how to lexically scope macros
evaluateCall (ValMacro (Stack _defnstack) _ labels body) argsE = do
  (Stack callerstack) <- gets envCallStack
  !args <- mapM interpretMacroArgs argsE
  !argsScope <- argsToScope labels args
  !result <- closure (Stack $ argsScope : callerstack) $ interpretExpression body
  case result of
    ValQuoted expr -> interpretExpression $ unwrapQuotes expr
    _ -> pure result
  where
    interpretMacroArgs :: Expr -> Evaluator m EvalValue
    interpretMacroArgs (ExprLiteral _ lit) = interpretLiteral lit
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
