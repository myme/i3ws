module Test.MockTree where

import Control.Applicative
import I3.Tree
import Test.QuickCheck

newtype MockTree = MockTree { getMockTree :: Node } deriving Show

instance Arbitrary MockTree where
  arbitrary = MockTree <$> arbitraryRoot

getId :: Gen Int
getId = getPositive <$> arbitrary

-- Root -> [Output] -> { Dock, Content -> [Workspace] -> [Window] }
arbitraryRoot :: Gen Node
arbitraryRoot = do
  id'   <- getId
  nodes <- scale (min 2) $ listOf arbitraryOutput
  pure (Node id' (Just "root") Root nodes [] Nothing Nothing)

arbitraryOutput :: Gen Node
arbitraryOutput = do
  id'   <- getId
  name' <- arbitrary
  nodes <- arbitraryContent
  pure (Node id' (Just name') Output [nodes] [] Nothing Nothing)

arbitraryDock :: Gen Node
arbitraryDock = do
  id'   <- getId
  name' <- arbitrary
  dock  <- arbitraryWindow False
  pure (Node id' (Just name') Dockarea [dock] [] Nothing Nothing)

arbitraryContent :: Gen Node
arbitraryContent = do
  id' <- getId
  wss <- scale (min 4) arbitraryWorkspaces
  pure (Node id' (Just "content") Con wss [] Nothing Nothing)

arbitraryWorkspaces :: Gen [Node]
arbitraryWorkspaces = fmap (zipWith nameWorkspace [1 ..]) . listOf $ do
  id'   <- getId
  let name' = Nothing -- Named by nameWorkspace
  nodes    <- scale (min 3) $ listOf (arbitraryWindow False)
  floating <- scale (min 3) $ listOf (arbitraryWindow True)
  pure (Node id' name' Workspace nodes floating Nothing Nothing)
  where nameWorkspace i w = w { node_name = Just (show (i :: Int)) }

arbitraryWindow :: Bool -> Gen Node
arbitraryWindow floating = do
  id'   <- getId
  name' <- arbitrary
  let type' = if floating then FloatingCon else Con
  win       <- Just . getPositive <$> arbitrary
  win_props <- Just <$> liftA3 WindowProps arbitrary arbitrary arbitrary
  pure (Node id' (Just name') type' [] [] win win_props)
