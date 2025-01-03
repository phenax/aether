module Aether.Runtime where

import Aether.Runtime.Interpreter (interpretAll, interpretExpression, runExprEvaluatorWithCallStack, runExprInterpreterRaw, runInterpreterRaw)
import Aether.Syntax.Parser (parseAll)
import Aether.Types
import Control.Monad (foldM, forM)
import Control.Monad.IO.Class (MonadIO)
import Data.FileEmbed (embedFile)
import qualified Data.Text as Text
import Language.Haskell.TH (runIO)
import Language.Haskell.TH.Syntax (Lift (lift), Q (Q))
import System.Exit (exitFailure)
import Text.Megaparsec (errorBundlePretty)

runExprInterpreter :: (MonadIO m) => Expr -> m (Either EvalError EvalValue)
runExprInterpreter = fmap fst . runExprInterpreterRaw standardLib

runInterpreter :: (MonadIO m) => [Expr] -> m (Either EvalError [EvalValue])
runInterpreter = fmap fst . runInterpreterRaw standardLib

standardLib :: EvalEnvironment
standardLib =
  $( let loadFile :: EvalEnvironment -> String -> Q EvalEnvironment
         loadFile environment file = do
           res <- runIO (readFile file)
           let parsedResult = parseAll "stdlib/core" $ Text.pack res
           case parsedResult of
             Right ast -> runIO (snd <$> runInterpreterRaw environment ast)
             Left e -> runIO $ do
               putStrLn (errorBundlePretty e)
               pure environment
      in foldM loadFile mempty ["./stdlib/core.rkt"] >>= lift
   )
