module Main where

import I3.Workspaces
import Test.Hspec
import Test.QuickCheck

newtype Alpha = Alpha String deriving Show

instance Arbitrary Alpha where
  arbitrary = Alpha <$> listOf (elements (['A'..'Z'] <> ['a'..'z']))

main :: IO ()
main = hspec $ do
  describe "I3.Workspaces" $ do
    describe "moveLeft" $ do
      it "moving leftmost is identity" $ do
        property $ \(NonEmpty ws) ->
          let ns = renumber ws
          in moveLeft (head ns) ns `shouldBe` ns

    describe "moveRight" $ do
      it "moving rightmost is identity" $ do
        property $ \(NonEmpty ws) ->
          let ns = renumber ws
          in moveRight (last ns) ns `shouldBe` ns

    describe "renumber" $ do
      it "is unchanged with sequenced numbers" $ do
        renumber ["1", "2", "3"] `shouldBe` ["1", "2", "3"]

      it "reorders unsorted numbers" $ do
        renumber ["2", "3", "1"] `shouldBe` ["1", "2", "3"]

      it "only changes number" $ do
        renumber ["2: foo", "3: bar", "1: baz"] `shouldBe` ["1: foo", "2: bar", "3: baz"]

      it "adds number to unnumbered workspaces" $ do
        renumber ["4: foo", "bar", "baz"] `shouldBe` ["1: foo", "2: bar", "3: baz"]

      it "rename whitespace name" $ do
        renumber [" "] `shouldBe` ["1"]

      it "has idempotence" $ do
        property $ \xs (Positive n) (Positive m) ->
          let renames = iterate renumber xs
          in renames !! n `shouldBe` renames !! m

    describe "parseName" $ do
      it "Always no number" $ do
        property $ \(Alpha s) ->
          parseName s `shouldBe` (Nothing, s)

      it "Just number" $ do
        property $ \(Positive n) -> parseName (show n) `shouldBe` (Just n, "")

      it "Just whitespace" $ do
        parseName "  " `shouldBe` (Nothing, "")

      it "Number and whitespace" $ do
        property $ \(Positive n) (Positive spaces) ->
          let s = replicate spaces ' '
          in parseName (show n <> s) `shouldBe` (Just n, "")

      it "With number, no colon" $ do
        property $ \(Alpha s) (Positive n) ->
          parseName (show n <> " " <> s) `shouldBe` (Just n, s)

      it "With number and colon" $ do
        property $ \(Alpha s) (Positive n) ->
          parseName (show n <> ": " <> s) `shouldBe` (Just n, s)
