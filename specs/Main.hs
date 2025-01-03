module Main (main) where

import qualified Specs.Integration.InterpreterSpec
import qualified Specs.InterpreterSpec
import qualified Specs.ParserSpec
import Test.Hspec (hspec)

main :: IO ()
main = hspec $ do
  Specs.ParserSpec.test
  Specs.InterpreterSpec.test
  Specs.Integration.InterpreterSpec.test
  pure ()
