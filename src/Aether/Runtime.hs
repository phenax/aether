module Aether.Runtime
  ( runExprInterpreter,
    runInterpreter,
    envWithStdLib,
    loadStdLib,
    evaluateFile,
    evaluator,
    interpret,
    runEvaluator,
  )
where

import Aether.Runtime.Interpreter (Interpretable (interpret), runEvaluator)
import Aether.Runtime.Scope (defineScriptMainPath)
import Aether.Syntax.Parser (parseAll)
import Aether.Types
import Control.Monad (foldM)
import Control.Monad.IO.Class (MonadIO (liftIO))
import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO
import Language.Haskell.TH (runIO)
import Language.Haskell.TH.Syntax (Lift (lift), Q)
import Text.Megaparsec (errorBundlePretty)

runExprInterpreter :: (MonadIO m) => Expr -> m (Either EvalError EvalValue)
runExprInterpreter expr = do
  env <- envWithStdLib
  fst <$> runEvaluator (interpret expr) env

runInterpreter :: (MonadIO m) => [Expr] -> m (Either EvalError [EvalValue])
runInterpreter exprs = do
  env <- envWithStdLib
  fst <$> runEvaluator (interpret exprs) env

envWithStdLib :: (MonadIO m) => m EvalEnvironment
envWithStdLib = snd <$> runEvaluator loadStdLib mempty

loadStdLib :: Evaluator m ()
loadStdLib = mapM_ (interpret :: Expr -> Evaluator m EvalValue) standardLib

evaluateFile :: (MonadIO m) => FilePath -> m (Either EvalError [EvalValue])
evaluateFile filePath = do
  code <- liftIO $ TextIO.readFile filePath
  let results = parseAll filePath code
  case results of
    Right exprs -> do
      let eval = do
            loadStdLib
            defineScriptMainPath filePath
            interpret exprs
      fst <$> runEvaluator eval mempty
    Left e -> error $ errorBundlePretty e

evaluator :: (MonadIO m) => EvalEnvironment -> [Expr] -> m (Either EvalError [EvalValue], EvalEnvironment)
evaluator env exprs = runEvaluator (interpret exprs) env

-- Load and parse the standard library at compile time
standardLib :: [Expr]
standardLib =
  $( do
       let loadCodeInEnv :: [Expr] -> String -> Q [Expr]
           loadCodeInEnv parsed file = do
             result <- runIO $ parseAll file . Text.pack <$> readFile file
             case result of
               Right ast -> pure $ parsed ++ ast
               Left e -> error $ errorBundlePretty e
           libFiles = ["./stdlib/core.scm", "./stdlib/list.scm", "./stdlib/result.scm"]

       foldM loadCodeInEnv [] libFiles >>= lift
   )
