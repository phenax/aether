module Main (main) where

import qualified Specs.Integration.BuiltinsSpec
import qualified Specs.Integration.InterpreterSpec
import qualified Specs.Integration.StdlibSpec
import qualified Specs.InterpreterSpec
import qualified Specs.ParserSpec
import qualified Specs.ScopeSpec
import Test.Hspec (hspec)

main :: IO ()
main = hspec $ do
  Specs.ParserSpec.test
  Specs.InterpreterSpec.test
  Specs.Integration.InterpreterSpec.test
  Specs.Integration.StdlibSpec.test
  Specs.Integration.BuiltinsSpec.test
  Specs.ScopeSpec.test
  pure ()
