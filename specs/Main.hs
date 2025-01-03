module Main (main) where

import qualified Specs.InterpreterSpec
import qualified Specs.ParserSpec
import Test.Hspec (hspec)

main :: IO ()
main = hspec $ do
  Specs.ParserSpec.test
  Specs.InterpreterSpec.test
  pure ()
