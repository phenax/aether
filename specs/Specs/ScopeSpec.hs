module Specs.ScopeSpec where

import Aether.Runtime.Interpreter (runEvaluator)
import Aether.Runtime.Scope (closure, defineInCurrentScope, updateSymbolValue)
import Aether.Types
import Control.Monad.RWS (gets, modify')
import Data.Bifunctor (Bifunctor (second))
import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import Test.Hspec

test :: SpecWith ()
test = do
  let toStackList = stack . envCallStack
  let eval env f = second toStackList <$> runEvaluator f env
  let addValueToScope n v s@(Scope {scopeTable}) = s {scopeTable = Map.insert n v scopeTable}

  describe "#updateSymbolValue" $ do
    context "when symbol doesnt exist" $ do
      it "does defines symbol in current scope" $ do
        let rootScope = Scope (ScopeId 0) $ Map.fromList [("root", ValNil)]
        let secondScope = Scope (ScopeId 1) $ Map.fromList [("second", ValNil)]
        let env = mempty {envCallStack = Stack $ Seq.fromList [rootScope, secondScope]}
        toStackList (updateSymbolValue "new" (ValNumber 42) env)
          `shouldBe` Seq.fromList [rootScope, addValueToScope "new" (ValNumber 42) secondScope]

  describe "#closure" $ do
    it "evaluates within closure with appended scope" $ do
      let rootScope = Scope (ScopeId 0) $ Map.fromList [("root", ValNumber 9)]
      let newValuesScope = Scope (ScopeId 2) $ Map.fromList [("closure", ValNumber 2)]
      let closureScope = Scope (ScopeId 1) $ Map.fromList [("target", ValNumber 5)]
      let closureStack = Stack $ Seq.fromList [rootScope, closureScope]
      let callerStack = Stack $ Seq.fromList [rootScope]
      let env = mempty {envCallStack = callerStack}

      (evaluationStack, postEvaluationStack) <-
        eval env $ closure newValuesScope closureStack $ gets toStackList

      evaluationStack `shouldBe` Right (Seq.fromList [rootScope, closureScope, newValuesScope])
      postEvaluationStack `shouldBe` Seq.fromList [rootScope]

    context "when symbols are defined inside closure" $ do
      it "defines values at the top of the stack during evaluation" $ do
        let rootScope = Scope (ScopeId 0) $ Map.fromList [("root", ValNumber 9)]
        let newValuesScope = Scope (ScopeId 2) $ Map.fromList [("closure", ValNumber 2)]
        let closureScope = Scope (ScopeId 1) $ Map.fromList [("target", ValNumber 5)]
        let closureStack = Stack $ Seq.fromList [rootScope, closureScope]
        let callerStack = Stack $ Seq.fromList [rootScope]
        let env = mempty {envCallStack = callerStack}

        (evaluationStack, postEvaluationStack) <-
          eval env $ closure newValuesScope closureStack $ do
            modify' $ defineInCurrentScope "inside" ValNil
            gets toStackList

        evaluationStack `shouldBe` Right (Seq.fromList [rootScope, closureScope, addValueToScope "inside" ValNil newValuesScope])
        postEvaluationStack `shouldBe` Seq.fromList [rootScope]

    context "when external symbols are updated from within closure" $ do
      it "persists updated value outside closure" $ do
        let rootScope = Scope (ScopeId 0) $ Map.fromList [("root", ValNumber 9)]
        let newValuesScope = Scope (ScopeId 2) $ Map.fromList [("closure", ValNumber 2)]
        let closureScope = Scope (ScopeId 1) $ Map.fromList [("target", ValNumber 5)]
        let closureStack = Stack $ Seq.fromList [rootScope, closureScope]
        let callerStack = Stack $ Seq.fromList [rootScope]
        let env = mempty {envCallStack = callerStack}

        (evaluationStack, postEvaluationStack) <-
          eval env $ closure newValuesScope closureStack $ do
            modify' $ updateSymbolValue "root" $ ValNumber 21
            gets toStackList

        let updatedRootScope = addValueToScope "root" (ValNumber 21) rootScope
        evaluationStack `shouldBe` Right (Seq.fromList [updatedRootScope, closureScope, newValuesScope])
        postEvaluationStack `shouldBe` Seq.fromList [updatedRootScope]
