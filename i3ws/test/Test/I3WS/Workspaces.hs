module Test.I3WS.Workspaces where

import Data.Foldable
import I3
import I3.Workspaces
import I3WS.Types
import I3WS.Workspaces
import Test.Hspec
import Test.I3WS.Mock
import Test.QuickCheck

newtype Alpha = Alpha { getAlpha :: String } deriving Show

instance Arbitrary Alpha where
  arbitrary = Alpha <$> listOf (elements (['A'..'Z'] <> ['a'..'z']))

tests :: Spec
tests =
  describe "I3WS.Workspaces" $ do

    describe "getWorkspaces" $
      it "get empty list of workspaces" $ do
        ws <- runMock getWorkspaces
        liftIO (map name ws `shouldBe` [])

    let createWorkspaces wss = do
          mock <- i3ws_invoker <$> ask
          traverse_ (createWorkspace mock) wss
          assignNumbers

    describe "createWorkspace" $ do
      it "creates a new workspace with number" $ do
        ws <- runMock $ \mock -> do
          createWorkspaces ["foo"]
          getWorkspaces mock
        map name ws `shouldBe` ["1:foo"]

      it "creates multiple workspaces with increasing numbers" $ do
        ws <- runMock $ \mock -> do
          createWorkspaces ["foo", "bar"]
          getWorkspaces mock
        map name ws `shouldBe` ["1:foo", "2:bar"]

    describe "moveRight" $ do
      it "move last is identity" $ do
        ws <- runMock $ \mock -> do
          createWorkspaces ["foo", "bar"]
          moveRight
          getWorkspaces mock
        map name ws `shouldBe` ["1:foo", "2:bar"]

      it "move left then right is identity" $ do
        ws <-runMock $ \mock -> do
          createWorkspaces ["foo", "bar"]
          moveLeft
          moveRight
          getWorkspaces mock
        map name ws `shouldBe` ["1:foo", "2:bar"]

    describe "moveLeft" $ do
      it "moves last to first" $ do
        ws <- runMock $ \mock -> do
          createWorkspaces ["foo", "bar", "baz"]
          moveLeft
          moveLeft
          getWorkspaces mock
        map name ws `shouldBe` ["1:baz", "2:foo", "3:bar"]

    describe "renumber" $ do
      it "is unchanged with sequenced numbers" $
        renumber ":" ["1", "2", "3"] `shouldBe` ["1", "2", "3"]

      it "reorders unsorted numbers" $
        renumber ":" ["2", "3", "1"] `shouldBe` ["1", "2", "3"]

      it "only changes number" $
        renumber ":" ["2:foo", "3:bar", "1:baz"] `shouldBe` ["1:foo", "2:bar", "3:baz"]

      it "adds number to unnumbered workspaces" $
        renumber ":" ["4:foo", "bar", "baz"] `shouldBe` ["1:foo", "2:bar", "3:baz"]

      it "rename whitespace name" $
        renumber ":" [" "] `shouldBe` ["1"]

      it "has idempotence" $
        property $ \xs (Positive n) (Positive m) ->
          let renames = iterate (renumber ":") xs
          in renames !! n `shouldBe` renames !! m

    describe "parseName" $ do
      it "Always no number" $
        property $ \(Alpha s) ->
          parseName ":" s `shouldBe` (Nothing, s)

      it "Just number" $
        property $ \(Positive n) -> parseName ":" (show n) `shouldBe` (Just n, "")

      it "Just whitespace" $
        parseName ":" "  " `shouldBe` (Nothing, "")

      it "Number and whitespace" $
        property $ \(Positive n) (Positive spaces) ->
          let s = replicate spaces ' '
          in parseName ":" (show n <> s) `shouldBe` (Just n, "")

      it "With number, no colon" $
        property $ \(Alpha s) (Positive n) ->
          parseName ":" (show n <> " " <> s) `shouldBe` (Just n, s)

      it "With number and colon" $
        property $ \(Alpha s) (Positive n) ->
          parseName ":" (show n <> ": " <> s) `shouldBe` (Just n, s)
