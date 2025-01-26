module Main (main) where

import qualified Aether.Repl as Repl
import qualified Aether.Runtime as Runtime
import Aether.Runtime.Value (evalErrorToValue, showCode)
import Aether.Types
import Data.Default (Default (def))
import qualified System.Environment

data Action = Repl | RunScript FilePath
  deriving (Show, Eq)

data Config = Config
  { configAction :: Maybe Action,
    configHelp :: Bool
  }
  deriving (Show, Eq)

instance Default Config where
  def = Config {configAction = Nothing, configHelp = False}

main :: IO ()
main = do
  args <- System.Environment.getArgs
  case parseArgs args def of
    Config {configHelp = True} -> putStrLn "Help yourself"
    Config {configAction = Just (RunScript scriptFile)} -> Runtime.evaluateFile scriptFile >>= throwIfError
    Config {configAction = Just Repl} -> Runtime.envWithStdLib >>= Repl.runRepl
    _ -> pure ()
  where
    throwIfError :: Either EvalError [EvalValue] -> IO ()
    throwIfError (Right _val) = pure ()
    throwIfError (Left e) = error $ showCode $ evalErrorToValue e

parseArgs :: [String] -> Config -> Config
parseArgs ("repl" : _) config = config {configAction = Just Repl}
parseArgs ("run" : file : _) config = config {configAction = Just $ RunScript file}
parseArgs ("-h" : _) config = config {configHelp = True}
parseArgs ("--help" : _) config = config {configHelp = True}
parseArgs [] config = config
parseArgs (_ : _) config = config {configHelp = True}
