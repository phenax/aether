module Main (main) where

import qualified Aether.Runtime as Runtime
import Aether.Runtime.Value (evalErrorToValue, showEvalValue)
import qualified Aether.Syntax.Parser as Parser
import Aether.Types
import Data.Default (Default (def))
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO
import qualified System.Environment
import System.Exit (exitSuccess)
import System.IO (hFlush, stdout)
import Text.Megaparsec (errorBundlePretty)

data Action = Repl | RunScript FilePath
  deriving (Show, Eq)

data Configuration = Configuration
  { configAction :: Maybe Action,
    configHelp :: Bool
  }
  deriving (Show, Eq)

instance Default Configuration where
  def = Configuration {configAction = Nothing, configHelp = False}

main :: IO ()
main = do
  args <- System.Environment.getArgs
  case parseArgs args def of
    Configuration {configHelp = True} -> putStrLn "Help"
    Configuration {configAction = Just (RunScript scriptFile)} -> do
      Runtime.evaluateFile scriptFile >>= throwIfError
    Configuration {configAction = Just Repl} -> do
      env <- Runtime.envWithStdLib
      runRepl env
    _ -> pure ()
  where
    throwIfError :: Either EvalError [EvalValue] -> IO ()
    throwIfError (Right _val) = pure ()
    throwIfError (Left e) = error $ showEvalValue $ evalErrorToValue e

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
    evalCommand _ = pure ()

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
    printResult (Right val) = putStr . unlines $ fmap showEvalValue val
    printResult (Left e) = putStrLn . showEvalValue $ evalErrorToValue e

parseArgs :: [String] -> Configuration -> Configuration
parseArgs ("repl" : _) config = config {configAction = Just Repl}
parseArgs ("run" : file : _) config = config {configAction = Just $ RunScript file}
parseArgs ("-h" : _) config = config {configHelp = True}
parseArgs ("--help" : _) config = config {configHelp = True}
parseArgs [] config = config
parseArgs (_ : _) config = config {configHelp = True}
