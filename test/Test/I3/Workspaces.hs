module Test.I3.Workspaces where

import Data.Foldable (traverse_)
import I3.Workspaces
import Test.Hspec
import Test.I3.Mock
import Test.QuickCheck hiding (output)

newtype Alpha = Alpha { getAlpha :: String } deriving Show

instance Arbitrary Alpha where
  arbitrary = Alpha <$> listOf (elements (['A'..'Z'] <> ['a'..'z']))

tests :: Spec
tests =
  describe "I3.Workspaces" $ do
    describe "getWorkspaces" $
      it "get empty list of workspaces" $ do
        mock <- defaultMock
        ws <- getWorkspaces mock
        ws `shouldBe` []

    let createWorkspaces mock = traverse_ (createWorkspace mock)

    describe "createWorkspace" $ do
      it "creates a new workspace with number" $ do
        mock <- defaultMock
        createWorkspaces mock ["foo"]
        ws <- getWorkspaces mock
        map name ws `shouldBe` ["1:foo"]

      it "creates multiple workspaces with increasing numbers" $ do
        mock <- defaultMock
        createWorkspaces mock ["foo", "bar"]
        ws <- getWorkspaces mock
        map name ws `shouldBe` ["1:foo", "2:bar"]

    describe "moveRight" $ do
      it "move last is identity" $ do
        mock <- defaultMock
        createWorkspaces mock ["foo", "bar"]
        moveRight mock
        ws <- getWorkspaces mock
        map name ws `shouldBe` ["1:foo", "2:bar"]

      it "move left then right is identity" $ do
        mock <- defaultMock
        createWorkspaces mock ["foo", "bar"]
        moveLeft mock
        moveRight mock
        ws <- getWorkspaces mock
        map name ws `shouldBe` ["1:foo", "2:bar"]

    describe "moveLeft" $ do
      it "moves last to first" $ do
        mock <- defaultMock
        createWorkspaces mock ["foo", "bar", "baz"]
        moveLeft mock
        moveLeft mock
        ws <- getWorkspaces mock
        map name ws `shouldBe` ["1:baz", "2:foo", "3:bar"]

    describe "renumber" $ do
      it "is unchanged with sequenced numbers" $
        renumber ["1", "2", "3"] `shouldBe` ["1", "2", "3"]

      it "reorders unsorted numbers" $
        renumber ["2", "3", "1"] `shouldBe` ["1", "2", "3"]

      it "only changes number" $
        renumber ["2:foo", "3:bar", "1:baz"] `shouldBe` ["1:foo", "2:bar", "3:baz"]

      it "adds number to unnumbered workspaces" $
        renumber ["4:foo", "bar", "baz"] `shouldBe` ["1:foo", "2:bar", "3:baz"]

      it "rename whitespace name" $
        renumber [" "] `shouldBe` ["1"]

      it "has idempotence" $
        property $ \xs (Positive n) (Positive m) ->
          let renames = iterate renumber xs
          in renames !! n `shouldBe` renames !! m

    describe "parseName" $ do
      it "Always no number" $
        property $ \(Alpha s) ->
          parseName s `shouldBe` (Nothing, s)

      it "Just number" $
        property $ \(Positive n) -> parseName (show n) `shouldBe` (Just n, "")

      it "Just whitespace" $
        parseName "  " `shouldBe` (Nothing, "")

      it "Number and whitespace" $
        property $ \(Positive n) (Positive spaces) ->
          let s = replicate spaces ' '
          in parseName (show n <> s) `shouldBe` (Just n, "")

      it "With number, no colon" $
        property $ \(Alpha s) (Positive n) ->
          parseName (show n <> " " <> s) `shouldBe` (Just n, s)

      it "With number and colon" $
        property $ \(Alpha s) (Positive n) ->
          parseName (show n <> ": " <> s) `shouldBe` (Just n, s)
