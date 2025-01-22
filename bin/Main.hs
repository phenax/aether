module Main where

import qualified Aether.Runtime as Runtime
import Aether.Runtime.Value (evalErrorToValue, showEvalValue)
import qualified Aether.Syntax.Parser as Parser
import Aether.Types
import Control.Monad.IO.Class (MonadIO)
import Data.Default (Default (def))
import Data.Text (Text)
import qualified Data.Text.IO as TextIO
import qualified System.Environment
import Text.Megaparsec (errorBundlePretty)

main :: IO ()
main = do
  args <- System.Environment.getArgs
  case parseArgs args def of
    Configuration {configHelp = True} -> putStrLn "Help"
    Configuration {configScriptFile = Just scriptFile} -> do
      code <- TextIO.readFile scriptFile
      evalExpr code >>= handleResult
    _ -> pure ()

handleResult :: Either EvalError [EvalValue] -> IO ()
handleResult (Right _val) = pure () -- mapM_ (putStrLn . showEvalValue) val
handleResult (Left e) = error $ showEvalValue $ evalErrorToValue e

evalExpr :: (MonadIO m) => Text -> m (Either EvalError [EvalValue])
evalExpr code = do
  let results = Parser.parseAll "input" code
  case results of
    Right exprs -> Runtime.runInterpreter exprs
    Left e -> error $ errorBundlePretty e

data Configuration = Configuration
  { configScriptFile :: Maybe FilePath,
    configHelp :: Bool
  }
  deriving (Show, Eq)

instance Default Configuration where
  def = Configuration {configScriptFile = Nothing, configHelp = False}

parseArgs :: [String] -> Configuration -> Configuration
parseArgs ("-f" : file : _) config = config {configScriptFile = Just file}
parseArgs ("-h" : _) config = config {configHelp = True}
parseArgs ("--help" : _) config = config {configHelp = True}
parseArgs _ config = config
