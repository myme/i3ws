{-# LANGUAGE OverloadedStrings #-}

module Test.I3 where

import Control.Monad
import Data.Aeson (encode)
import Data.ByteString.Lazy hiding (map)
import I3.IPC hiding (Output, Workspace)
import I3.Tree
import Test.Hspec
import Test.QuickCheck

newtype Mock = StaticResponse ByteString

instance Invoker Mock where
  invoke (StaticResponse tree) (Request GetTree _) = pure (Response Tree tree)
  invoke _ _ = undefined
  subscribe = undefined

newtype RootNode = RootNode { getRootNode :: Node } deriving Show

instance Arbitrary RootNode where
  arbitrary = sized (fmap mkroot . arbitraryNode)
    where mkroot n = RootNode (n { node_type = Root })

arbitraryNode :: Int -> Gen Node
arbitraryNode sz = do
  id'   <- arbitrary
  name' <- arbitrary
  type' <- if isLeaf
    then elements [Con, FloatingCon, Dockarea]
    else elements [Con, Output, Workspace]
  nodes     <- nodeList
  floating  <- nodeList
  win       <- if isLeaf then arbitrary else pure Nothing
  win_props <- if isLeaf
    then Just <$> liftM3 WindowProps arbitrary arbitrary arbitrary
    else pure Nothing
  pure (Node id' name' type' nodes floating win win_props)
  where
    isLeaf = sz == 0
    sz' = round ((fromIntegral sz :: Double) / 5)
    nodeList = if isLeaf
      then pure []
      else replicateM sz' (arbitraryNode sz')

tests :: Spec
tests = do
  describe "Tree" $ do
    describe "getTree" $ do
      it "returns current tree" $
        property $ \(RootNode node) -> do
          root <- getTree (StaticResponse (encode node))
          root `shouldBe` node

    describe "flatten" $ do
      it "always returns root as first element" $ do
        property $ \(RootNode node) -> do
          let first:_ = flatten node
          first `shouldBe` node
