module Test.I3 where

import Control.Exception
import Data.Aeson
import Data.Either
import I3.Command
import I3.IPC hiding (Output, Workspace)
import I3.Tree
import Test.Hspec
import Test.MockTree
import Test.QuickCheck

staticCmd :: ToJSON a => a -> Invoker IO
staticCmd res = Invoker
  { getInvoker = \case
      (Request Command _) -> pure (Response Command (eitherDecode (encode res)))
      _                   -> undefined
  , getSubscriber = undefined
  }

staticTree :: ToJSON a => a -> Invoker IO
staticTree res = Invoker
  { getInvoker = \case
      (Request Tree _) -> pure (Response Tree (eitherDecode (encode res)))
      _                -> undefined
  , getSubscriber = undefined
  }

tests :: Spec
tests = do
  describe "Command" $ do
    describe "command" $ do
      let try' :: IO a -> IO (Either I3Error a)
          try' = try

      it "checks success" $ do
        let success = [object ["success" .= True]]
        res <- try' $ command (staticCmd success) "foo bar"
        res `shouldBe` Right ()

      it "throws error" $ do
        let reply = [object ["success" .= False, "error" .= ("some error" :: Value)]]
        res <- try' $ command (staticCmd reply) "foo bar"
        res `shouldBe` Left (CommandFailed "some error")

      it "fails without array response" $ do
        let success = object ["success" .= True]
        res <- try' $ command (staticCmd success) "foo bar"
        res `shouldSatisfy` isLeft

      it "fails with multiple responses" $ do
        let success = [object ["success" .= True], object ["success" .= True]]
        res <- try' $ command (staticCmd success) "foo bar"
        res `shouldSatisfy` isLeft

  describe "Tree" $ do
    describe "getTree" $ do
      it "returns current tree" $
        property $ \(MockTree node) -> do
          root <- getTree (staticTree node)
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

    describe "workspaces" $ do
      it "always returns Workspace" $ do
        property $ \(MockTree node) -> do
          workspaces node `shouldSatisfy` all ((== Workspace) . node_type)
