{-# LANGUAGE OverloadedStrings #-}

module Test.I3 where

import           Data.Aeson (encode)
import qualified Data.ByteString.Lazy as B
import           I3.IPC hiding (Output, Workspace)
import           I3.Tree
import           Test.Hspec
import           Test.MockTree
import           Test.QuickCheck

newtype Mock = StaticResponse B.ByteString

instance Invoker Mock where
  invoke (StaticResponse tree) (Request GetTree _) = pure (Response Tree tree)
  invoke _ _ = undefined
  subscribe = undefined

tests :: Spec
tests = do
  describe "Tree" $ do
    describe "getTree" $ do
      it "returns current tree" $
        property $ \(MockTree node) -> do
          root <- getTree (StaticResponse (encode node))
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
