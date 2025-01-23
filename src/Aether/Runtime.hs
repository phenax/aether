module Aether.Runtime where

import Aether.Runtime.Interpreter (runExprInterpreterRaw, runInterpreterRaw)
import Aether.Syntax.Parser (parseAll)
import Aether.Types
import Control.Monad (foldM)
import Control.Monad.IO.Class (MonadIO)
import qualified Data.Text as Text
import Language.Haskell.TH (runIO)
import Language.Haskell.TH.Syntax (Lift (lift), Q)
import Text.Megaparsec (errorBundlePretty)

runExprInterpreter :: (MonadIO m) => Expr -> m (Either EvalError EvalValue)
runExprInterpreter expr = do
  env <- envWithStdLib
  fst <$> runExprInterpreterRaw env expr

runInterpreter :: (MonadIO m) => [Expr] -> m (Either EvalError [EvalValue])
runInterpreter exprs = do
  env <- envWithStdLib
  fst <$> runInterpreterRaw env exprs

envWithStdLib :: (MonadIO m) => m EvalEnvironment
envWithStdLib = snd <$> runInterpreterRaw mempty standardLib

evaluator :: (MonadIO m) => EvalEnvironment -> [Expr] -> m (Either EvalError [EvalValue], EvalEnvironment)
evaluator = runInterpreterRaw

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
