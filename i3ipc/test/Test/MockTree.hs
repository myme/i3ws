module Test.MockTree where

import Control.Applicative
import I3.Tree
import Test.QuickCheck

newtype MockTree = MockTree { getMockTree :: Node } deriving Show

instance Arbitrary MockTree where
  arbitrary = MockTree <$> arbitraryRoot

getId :: Gen Int
getId = getPositive <$> arbitrary

-- | Generator of lists of max @sz@ entries.
listOfMax :: Int -> Gen a -> Gen [a]
listOfMax sz = scale (min sz) . listOf

-- | Create arbitrary I3 mock tree structure, starting with the root node.
-- |
-- | The tree should be structured somewhat like the following:
-- |
-- | Root -> [Output] -> { Dock | Content -> [Workspace] -> [Window] }
arbitraryRoot :: Gen Node
arbitraryRoot = do
  id'   <- getId
  nodes <- listOfMax 2 arbitraryOutput
  pure (Node id' (Just "root") Root nodes [] Nothing Nothing)

arbitraryOutput :: Gen Node
arbitraryOutput = do
  id'     <- getId
  name'   <- arbitrary
  dock    <- listOfMax 1 arbitraryDock
  content <- pure <$> arbitraryContent
  pure (Node id' (Just name') Output (dock <> content) [] Nothing Nothing)

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
  id'      <- getId
  nodes    <- listOfMax 3 (arbitraryWindow False)
  floating <- listOfMax 3 (arbitraryWindow True)
  let name' = Nothing -- Named by nameWorkspace
  pure (Node id' name' Workspace nodes floating Nothing Nothing)
  where nameWorkspace i w = w { node_name = Just (show (i :: Int)) }

arbitraryWindow :: Bool -> Gen Node
arbitraryWindow floating = do
  id'       <- getId
  name'     <- arbitrary
  win       <- Just <$> getId
  win_props <- Just <$> liftA3 WindowProps arbitrary arbitrary arbitrary
  let type' = if floating then FloatingCon else Con
  pure (Node id' (Just name') type' [] [] win win_props)
