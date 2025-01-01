module Main (main) where

import qualified Specs.ParserSpec
import Test.Hspec (hspec)

main :: IO ()
main = hspec $ do
  Specs.ParserSpec.test
  pure ()
