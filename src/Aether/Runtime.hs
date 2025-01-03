module Aether.Runtime where

import Aether.Runtime.Interpreter (interpretAll, interpretExpression, runExprEvaluatorWithCallStack, runExprInterpreterRaw, runInterpreterRaw)
import Aether.Syntax.Parser (ParserResult, parseAll)
import Aether.Types
import Control.Monad (foldM, forM)
import Control.Monad.IO.Class (MonadIO)
import qualified Data.Text as Text
import Language.Haskell.TH (runIO)
import Language.Haskell.TH.Syntax (Lift (lift), Q (Q))
import Text.Megaparsec (ParseErrorBundle (ParseErrorBundle), errorBundlePretty)

runExprInterpreter :: (MonadIO m) => Expr -> m (Either EvalError EvalValue)
runExprInterpreter = fmap fst . runExprInterpreterRaw standardLib

runInterpreter :: (MonadIO m) => [Expr] -> m (Either EvalError [EvalValue])
runInterpreter = fmap fst . runInterpreterRaw standardLib

-- Load, parse and interpret the standard library at compile time
standardLib :: EvalEnvironment
standardLib =
  $( do
       let evaluateAst :: EvalEnvironment -> ParserResult [Expr] -> Q EvalEnvironment
           evaluateAst environment (Right ast) = runIO $ snd <$> runInterpreterRaw environment ast
           evaluateAst environment (Left e) = error $ errorBundlePretty e

       let loadCodeInEnv :: EvalEnvironment -> String -> Q EvalEnvironment
           loadCodeInEnv env file = do
             result <- runIO $ parseAll file . Text.pack <$> readFile file
             evaluateAst env result

       foldM loadCodeInEnv mempty ["./stdlib/core.rkt"] >>= lift
   )
