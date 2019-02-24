module Main where

import Test.Hspec
import I3.Workspaces
import Test.QuickCheck
-- import Control.Exception (evaluate)

main :: IO ()
main = hspec $ do
  describe "I3.Workspaces" $ do
    describe "renumberWorkspaces" $ do
      it "is unchanged with sequenced numbers" $ do
        renumberWorkspaces ["1", "2", "3"] `shouldBe` ["1", "2", "3"]

      it "reorders unsorted numbers" $ do
        renumberWorkspaces ["2", "3", "1"] `shouldBe` ["1", "2", "3"]

      it "only changes number" $ do
        renumberWorkspaces ["2: foo", "3: bar", "1: baz"] `shouldBe` ["1: foo", "2: bar", "3: baz"]

      it "adds number to unnumbered workspaces" $ do
        renumberWorkspaces ["4: foo", "bar", "baz"] `shouldBe` ["1: foo", "2: bar", "3: baz"]

      it "rename whitespace name" $ do
        renumberWorkspaces [" "] `shouldBe` ["1"]

      it "has idempotence" $ do
        property $ \xs (Positive n) (Positive m) ->
          let renames = iterate renumberWorkspaces xs
          in renames !! n `shouldBe` renames !! m

    describe "workspaceNumber" $ do
      let noNumString = listOf $ elements (['A'..'Z'] <> ['a'..'z'])

      it "Always no number" $ do
        property $ forAll noNumString $ \s ->
          workspaceNumber s `shouldBe` (Nothing, s)

      it "Just number" $ do
        property $ \(Positive n) -> workspaceNumber (show n) `shouldBe` (Just n, "")

      it "Just whitespace" $ do
        workspaceNumber "  " `shouldBe` (Nothing, "")

      it "Number and whitespace" $ do
        property $ \(Positive n) (Positive spaces) ->
          let s = replicate spaces ' '
          in workspaceNumber (show n <> s) `shouldBe` (Just n, "")

      it "With number, no colon" $ do
        property $ forAll noNumString $ \s (Positive n) ->
          workspaceNumber (show n <> " " <> s) `shouldBe` (Just n, s)

      it "With number and colon" $ do
        property $ forAll noNumString $ \s (Positive n) ->
          workspaceNumber (show n <> ": " <> s) `shouldBe` (Just n, s)
