module Aether.Runtime.Builtins where

import Aether.Runtime.Scope (closure, defineInCurrentScope, mkScope, updateSymbolValue)
import Aether.Runtime.Value
import Aether.Types
import Control.Monad (forM_, (>=>))
import Control.Monad.Except (MonadError (catchError, throwError))
import Control.Monad.State.Strict (MonadState, gets, modify')
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import GHC.IO.Exception (ExitCode (ExitFailure, ExitSuccess))

type Interpret m = Expr -> Evaluator m EvalValue

type Builtin m = Interpret m -> [Expr] -> Evaluator m EvalValue

builtins :: (MonadState EvalEnvironment m, MonadError EvalError m, MonadLangIO m) => (Expr -> Evaluator m EvalValue) -> m (Map.Map Name ([Expr] -> m EvalValue))
builtins interpret =
  pure $
    Map.fromList
      [ ("set", builtinSet interpret),
        ("->", builtinLambda interpret),
        ("define", builtinDefine interpret),
        ("defmacro", builtinDefmacro interpret),
        ("do", builtinDo interpret),
        ("progn", builtinProgn interpret),
        ("eval", builtinEval interpret),
        ("car", builtinCar interpret),
        ("cdr", builtinCdr interpret),
        ("cons", builtinCons interpret),
        ("+", operateOnExprs (ValNumber . sum . fmap valToNumber) interpret),
        ("*", operateOnExprs (ValNumber . product . fmap valToNumber) interpret),
        ("-", operateOnExprs (ValNumber . subtractList . fmap valToNumber) interpret),
        ("/", operateOnExprs (ValNumber . divideList . fmap valToNumber) interpret),
        ("lt?", builtinLessThan interpret),
        ("gt?", builtinGreaterThan interpret),
        ("lte?", builtinLessThanOrEqualTo interpret),
        ("gte?", builtinGreaterThanOrEqualTo interpret),
        ("eq?", operateOnExprs (ValBool . checkIfAllEqual) interpret),
        ("&&", operateOnExprs (ValBool . all valToBool) interpret),
        ("||", operateOnExprs (ValBool . any valToBool) interpret),
        ("type", builtinTypeOf interpret),
        ("display", builtinDisplay interpret),
        ("quote", builtinQuote interpret),
        ("try", builtinTry interpret),
        ("error!", builtinError interpret),
        ("displayNl", builtinDisplay interpret . (++ [ExprLiteral NullSpan $ LitString "\n"])),
        ("!", builtinExecCommand interpret),
        ("import", builtinLoadScript interpret),
        ("string", builtinString interpret),
        ("make-symbol", builtinMakeSymbol interpret),
        ("get-args", builtinGetArgs interpret),
        ("exit", builtinExit interpret)
      ]

operateOnExprs :: ([EvalValue] -> EvalValue) -> Builtin m
operateOnExprs fn interpret exprs = fn <$> mapM interpret exprs

builtinExit :: Builtin m
builtinExit interpret [expr] = do
  interpret expr >>= systemExit . floor . valToNumber
  pure ValNil
builtinExit _ args = throwError $ ArgumentLengthError True 1 (length args) "exit"

builtinQuote :: Builtin m
builtinQuote _ [expr] = pure $ ValQuoted expr
builtinQuote _ args = throwError $ ArgumentLengthError True 1 (length args) "quote"

builtinDisplay :: Builtin m
builtinDisplay interpret exprs = do
  let displayVal = putStringToScreen . showCode
  mapM_ (interpret >=> displayVal) exprs
  pure ValNil

builtinSet :: Builtin m
builtinSet interpret [ExprSymbol _ name, valueE] = do
  !value <- interpret valueE
  modify' $ updateSymbolValue name value
  pure ValNil
builtinSet _ (ExprSymbol _ _ : args) =
  do throwError $ ArgumentLengthError True 2 (length args) "set"
builtinSet _ args =
  do throwError $ TypeError "set" $ "Invalid call to set: " ++ showCode args

builtinDefine :: Builtin m
builtinDefine _ (ExprSymList sourceSpan (ExprSymbol _ name : argsE) : bodyE) = do
  lambda <- mkLambda sourceSpan argsE bodyE
  modify' $ defineInCurrentScope name lambda
  pure ValNil
builtinDefine interpret [ExprSymbol _ name, valueE] = do
  value <- interpret valueE
  modify' $ defineInCurrentScope name value
  pure ValNil
builtinDefine _ (ExprSymbol _ _ : args) =
  do throwError $ ArgumentLengthError True 2 (length args) "define"
builtinDefine _ args =
  do throwError $ TypeError "define" $ "Invalid call to define" ++ showCode args

builtinLambda :: Builtin m
builtinLambda _ (ExprSymList sourceSpan argsE : bodyE) =
  mkLambda sourceSpan argsE bodyE
builtinLambda _ args =
  do throwError $ TypeError "->" $ "Invalid call to ->: " ++ showCode args

mkLambda :: SourceSpan -> [Expr] -> [Expr] -> Evaluator m EvalValue
mkLambda sourceSpan argsE bodyE = do
  stack <- gets envCallStack
  pure $ ValLambda stack sourceSpan argLabels body
  where
    argLabels = argToLabel <$> argsE
    body = if length bodyE == 1 then head bodyE else ExprSymList NullSpan (ExprSymbol NullSpan "progn" : bodyE)

builtinDefmacro :: Builtin m
builtinDefmacro _ (ExprSymList sourceSpan (ExprSymbol _ name : argsE) : bodyE) = do
  stack <- gets envCallStack
  modify' (defineInCurrentScope name $ ValMacro stack sourceSpan argLabels body)
  pure ValNil
  where
    argLabels = argToLabel <$> argsE
    body = if length bodyE == 1 then head bodyE else ExprSymList NullSpan (ExprSymbol NullSpan "progn" : bodyE)
builtinDefmacro _ args = do throwError $ TypeError "defmacro" $ "Invalid call to defmacro" ++ showCode args

builtinDo :: Builtin m
builtinDo interpret body = do
  stack <- gets envCallStack
  !scope <- mkScope Map.empty
  !results <- closure scope stack $ do
    mapM interpret body
  pure $ case results of
    [] -> ValNil
    _ -> last results

builtinProgn :: Builtin m
builtinProgn interpret body = do
  results <- mapM interpret body
  pure $ case results of
    [] -> ValNil
    _ -> last results

builtinEval :: Builtin m
builtinEval interpret [expr] = do
  res <- interpret expr
  case res of
    ValQuoted quote -> interpret quote
    _ -> pure res
builtinEval _ exprs = do throwError $ ArgumentLengthError True 1 (length exprs) "eval"

builtinCar :: Builtin m
builtinCar interpret [expr] = do
  res <- interpret expr
  case res of
    ValQuoted (ExprSymList _ (first : _)) -> interpret first
    ValQuoted (ExprSymList _ []) -> pure ValNil
    ValQuoted quote -> interpret quote
    _ -> pure res
builtinCar _ exprs = do throwError $ ArgumentLengthError True 1 (length exprs) "car"

builtinCdr :: Builtin m
builtinCdr interpret [expr] = do
  res <- interpret expr
  case res of
    ValQuoted (ExprSymList _ [_]) -> pure ValNil
    ValQuoted (ExprSymList _ (_ : rest)) ->
      ValQuoted . ExprSymList NullSpan . fmap ExprValue <$> mapM interpret rest
    _ -> pure ValNil
builtinCdr _ exprs = do throwError $ ArgumentLengthError True 1 (length exprs) "cdr"

builtinCons :: Builtin m
builtinCons interpret [itemE, restE] = do
  item <- interpret itemE
  rest <- interpret restE
  case rest of
    ValQuoted (ExprSymList _ values) -> pure $ ValQuoted (ExprSymList NullSpan (ExprValue item : values))
    ValNil -> pure $ ValQuoted (ExprSymList NullSpan [ExprValue item])
    ValQuoted (ExprLiteral _ LitNil) -> pure $ ValQuoted (ExprSymList NullSpan [ExprValue item])
    ValQuoted (ExprValue ValNil) -> pure $ ValQuoted (ExprSymList NullSpan [ExprValue item])
    _ -> pure $ ValQuoted (ExprSymList NullSpan [ExprValue item, ExprValue rest])
builtinCons _ exprs = do throwError $ ArgumentLengthError True 2 (length exprs) "cons"

builtinLessThan :: Builtin m
builtinLessThan interpret [exp1, exp2] = numberBinaryOp interpret (\a -> ValBool . (a <)) exp1 exp2
builtinLessThan _ exprs = throwError $ ArgumentLengthError True 2 (length exprs) "lt?"

builtinGreaterThan :: Builtin m
builtinGreaterThan interpret [exp1, exp2] = numberBinaryOp interpret (\a -> ValBool . (a >)) exp1 exp2
builtinGreaterThan _ exprs = throwError $ ArgumentLengthError True 2 (length exprs) "gt?"

builtinLessThanOrEqualTo :: Builtin m
builtinLessThanOrEqualTo interpret [exp1, exp2] = numberBinaryOp interpret (\a -> ValBool . (a <=)) exp1 exp2
builtinLessThanOrEqualTo _ exprs = throwError $ ArgumentLengthError True 2 (length exprs) "lte?"

builtinGreaterThanOrEqualTo :: Builtin m
builtinGreaterThanOrEqualTo interpret [exp1, exp2] = numberBinaryOp interpret (\a -> ValBool . (a >=)) exp1 exp2
builtinGreaterThanOrEqualTo _ exprs = throwError $ ArgumentLengthError True 2 (length exprs) "gte?"

builtinTypeOf :: Builtin m
builtinTypeOf interpret [expr] = ValQuoted . ExprSymbol NullSpan . typeOfValue <$> interpret expr
builtinTypeOf _ exprs = throwError $ ArgumentLengthError True 1 (length exprs) "type"

builtinTry :: Builtin m
builtinTry interpret exprs = catchError (toRight <$> tryValue) (pure . toLeft . evalErrorToValue)
  where
    tryValue = interpret (ExprSymList NullSpan $ ExprSymbol NullSpan "do" : exprs)
    toRight v = ValQuoted $ ExprSymList NullSpan [ExprValue ValNil, ExprValue v]
    toLeft e = ValQuoted $ ExprSymList NullSpan [ExprValue e, ExprValue ValNil]

builtinError :: Builtin m
builtinError interpret (labelE : messageE : _) = do
  label <- interpret labelE
  message <- interpret messageE
  throwError $ UserError label message
builtinError interpret [labelE] = do
  label <- interpret labelE
  throwError $ UserError label ValNil
builtinError _ [] = throwError $ UserError (ValQuoted $ ExprSymbol NullSpan "error") ValNil

numberBinaryOp :: Interpret m -> (Double -> Double -> EvalValue) -> Expr -> Expr -> Evaluator m EvalValue
numberBinaryOp interpret fn exp1 exp2 = do
  v1 <- valToNumber <$> interpret exp1
  v2 <- valToNumber <$> interpret exp2
  pure $ fn v1 v2

toCommand :: EvalValue -> Maybe (String, [String])
toCommand (ValQuoted (ExprSymList _ (cmd : args))) = Just (exprToString cmd, map exprToString args)
toCommand _ = Nothing

builtinExecCommand :: Builtin m
builtinExecCommand interpret (cmdE : argsE) = do
  res <- interpret $ ExprQuoted NullSpan $ ExprSymList NullSpan (cmdE : argsE)
  case toCommand res of
    Just (cmd, args) -> do
      (exitCode, stdout, stderr) <- execCommand cmd args
      case exitCode of
        ExitFailure n ->
          throwError $
            UserError
              (ValQuoted $ ExprSymbol NullSpan "proc/non-zero-exit-code")
              (ValString $ "Process exited with status code " ++ show n ++ "\n" ++ Text.unpack stderr)
        ExitSuccess -> do
          pure
            . ValQuoted
            . ExprSymList NullSpan
            $ [ExprValue $ ValString $ Text.unpack stdout, ExprValue $ ValString $ Text.unpack stderr]
    Nothing -> throwError $ UnknownError "Evaluation error: list evaluated to non-list"
builtinExecCommand _ [] = throwError $ ArgumentLengthError False 1 0 "!"

builtinGetArgs :: Builtin m
builtinGetArgs _ _ =
  ValQuoted . ExprSymList NullSpan . fmap (ExprValue . ValString) <$> getArgs

builtinString :: Builtin m
builtinString interpret exprs = do
  values <- mapM interpret exprs
  pure $ ValString $ concatMap valToString values

builtinMakeSymbol :: Builtin m
builtinMakeSymbol interpret [expr] = do
  value <- interpret expr
  pure . ValQuoted . ExprSymbol NullSpan $ valToString value
builtinMakeSymbol _ args = throwError $ ArgumentLengthError True 1 (length args) "make-symbol"

builtinLoadScript :: Builtin m
builtinLoadScript interpret scriptPathsE = do
  forM_ scriptPathsE $ \scriptPathE -> do
    scriptPath <- valToString <$> interpret scriptPathE
    exprs <- loadScriptToAST scriptPath
    case exprs of
      Right ast -> mapM interpret ast
      Left err ->
        throwError $ UserError (ValQuoted $ ExprSymbol NullSpan "import-error") (ValString err)
  pure ValNil
