module Specs.ScopeSpec where

import Aether.Runtime.Interpreter (runExprEvaluatorWithCallStack)
import Aether.Runtime.Scope (closure, defineInCurrentScope, updateSymbolValue)
import Aether.Types
import Control.Monad.RWS (gets, modify')
import Data.Bifunctor (Bifunctor (second))
import qualified Data.Map.Strict as Map
import Test.Hspec

test :: SpecWith ()
test = do
  describe "#closure" $ do
    let toStackList = stack . envCallStack
    let eval env f = second toStackList <$> runExprEvaluatorWithCallStack f env
    let addValueToScope n v s@(Scope {scopeTable}) = s {scopeTable = Map.insert n v scopeTable}

    it "evaluates within closure with appended scope" $ do
      let rootScope = Scope (ScopeId 0) $ Map.fromList [("root", ValNumber 9)]
      let newValuesScope = Scope (ScopeId 2) $ Map.fromList [("closure", ValNumber 2)]
      let closureScope = Scope (ScopeId 1) $ Map.fromList [("target", ValNumber 5)]
      let closureStack = Stack [closureScope, rootScope]
      let callerStack = Stack [rootScope]
      let env = mempty {envCallStack = callerStack}

      (evaluationStack, postEvaluationStack) <- eval env $ closure newValuesScope closureStack $ gets toStackList

      evaluationStack `shouldBe` Right [newValuesScope, closureScope, rootScope]
      postEvaluationStack `shouldBe` [rootScope]

    context "when symbols are defined inside closure" $ do
      it "defines values at the top of the stack during evaluation" $ do
        let rootScope = Scope (ScopeId 0) $ Map.fromList [("root", ValNumber 9)]
        let newValuesScope = Scope (ScopeId 2) $ Map.fromList [("closure", ValNumber 2)]
        let closureScope = Scope (ScopeId 1) $ Map.fromList [("target", ValNumber 5)]
        let closureStack = Stack [closureScope, rootScope]
        let callerStack = Stack [rootScope]
        let env = mempty {envCallStack = callerStack}

        (evaluationStack, postEvaluationStack) <- eval env $ closure newValuesScope closureStack $ do
          modify' $ defineInCurrentScope "inside" ValNil
          gets toStackList

        evaluationStack `shouldBe` Right [addValueToScope "inside" ValNil newValuesScope, closureScope, rootScope]
        postEvaluationStack `shouldBe` [rootScope]

    context "when external symbols are updated from within closure" $ do
      it "persists updated value outside closure" $ do
        let rootScope = Scope (ScopeId 0) $ Map.fromList [("root", ValNumber 9)]
        let newValuesScope = Scope (ScopeId 2) $ Map.fromList [("closure", ValNumber 2)]
        let closureScope = Scope (ScopeId 1) $ Map.fromList [("target", ValNumber 5)]
        let closureStack = Stack [closureScope, rootScope]
        let callerStack = Stack [rootScope]
        let env = mempty {envCallStack = callerStack}

        (evaluationStack, postEvaluationStack) <- eval env $ closure newValuesScope closureStack $ do
          modify' $ updateSymbolValue "root" $ ValNumber 21
          gets toStackList

        let updatedRootScope = addValueToScope "root" (ValNumber 21) rootScope
        evaluationStack `shouldBe` Right [newValuesScope, closureScope, updatedRootScope]
        postEvaluationStack `shouldBe` [updatedRootScope]
