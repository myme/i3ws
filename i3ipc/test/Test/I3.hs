module Test.I3 where

import Data.Aeson (ToJSON, eitherDecode, encode)
import I3.IPC hiding (Output, Workspace)
import I3.Tree
import Test.Hspec
import Test.MockTree
import Test.QuickCheck

static :: ToJSON a => a -> Invoker
static res = Invoker
  { getInvoker = \case
      (Request Tree _) -> pure (Response Tree (eitherDecode (encode res)))
      _                -> undefined
  , getSubscriber = undefined
  }

tests :: Spec
tests = do
  describe "Tree" $ do
    describe "getTree" $ do
      it "returns current tree" $
        property $ \(MockTree node) -> do
          root <- getTree (static node)
          root `shouldBe` node

    describe "flatten" $ do
      it "always returns root as first element" $ do
        property $ \(MockTree node) -> do
          let first:_ = flatten node
          first `shouldBe` node

    describe "leaves" $ do
      it "always returns Con, FloatingCon" $ do
        property $ \(MockTree node) -> do
          leaves node `shouldSatisfy` all ((`elem` [Con, FloatingCon]) . node_type)
