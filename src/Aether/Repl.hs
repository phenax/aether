module Aether.Repl where

import qualified Aether.Runtime.Interpreter as Runtime
import Aether.Runtime.Value (LangShow (showCode), evalErrorToValue)
import qualified Aether.Syntax.Parser as Parser
import Aether.Types
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO
import System.Exit (exitSuccess)
import System.IO (hFlush, stdout)
import Text.Megaparsec (errorBundlePretty)

-- TODO: Make into evaluator?
runRepl :: EvalEnvironment -> IO ()
runRepl env = do
  putStr "λ " >> hFlush stdout
  code <- TextIO.getLine
  case Text.unpack code of
    ('\\' : cmd) -> evalCommand cmd >> runRepl env
    _ -> evalCode code >>= runRepl
  where
    evalCommand "q" = exitSuccess
    evalCommand "quit" = exitSuccess
    evalCommand ('$' : code) = evalCode (Text.pack ("($ " ++ code ++ ")")) >>= runRepl
    evalCommand c | c `elem` ["help", "h"] = do
      putStrLn "Any aether expression will be evaluated"
      putStrLn "Commands:"
      putStrLn "  \\h             help menu (or \\help)"
      putStrLn "  \\q             exit repl (or \\quit)"
      putStrLn "  \\ + 5 2        evaluates (+ 5 2) as aether expression"
      putStrLn "  \\$ 5 + 2 - 3   evaluates (- (+ 5 2) 3) in infix notation"
    evalCommand code = evalCode (Text.pack ("(" ++ code ++ ")")) >>= runRepl

    evalCode :: Text -> IO EvalEnvironment
    evalCode code = do
      let parsedResult = Parser.parseAll "input" code
      case parsedResult of
        Right exprs -> do
          (result, nextEnv) <- Runtime.runEvaluator (Runtime.interpret exprs) env
          printResult result
          pure nextEnv
        Left e -> env <$ putStrLn (errorBundlePretty e)

    printResult :: Either EvalError [EvalValue] -> IO ()
    printResult (Right val) = putStr . unlines $ fmap showCode val
    printResult (Left e) = putStrLn . showCode $ evalErrorToValue e
