module Main where

import I3.Workspaces
import Test.Hspec
import Test.QuickCheck

newtype Alpha = Alpha { getAlpha :: String } deriving Show

instance Arbitrary Alpha where
  arbitrary = Alpha <$> listOf (elements (['A'..'Z'] <> ['a'..'z']))

main :: IO ()
main = hspec $ do
  describe "I3.Workspaces" $ do
    describe "move" $ do
      it "moveLeft moving leftmost is identity" $ do
        property $ \(NonEmpty ws) ->
          let ns = renumber ws
          in moveLeft 0 ns `shouldBe` ns

      it "moveRight moving rightmost is identity" $ do
        property $ \(NonEmpty ws) ->
          let ns = renumber ws
          in moveRight (length ns - 1) ns `shouldBe` ns

      it "moveRight then moveLeft is identity" $ do
        property $ \(NonEmpty ws) ->
          let ns = renumber ws
          in moveLeft 1 (moveRight 0 ns) `shouldBe` ns

      it "moveLeft then moveRight is identity" $ do
        property $ \w (NonEmpty ws) ->
          let ns = renumber (w:ws)
              last' = length ns - 1
          in moveRight (last' - 1) (moveLeft last' ns) `shouldBe` ns

      it "moveLeft <n> times places last first" $ do
        property $ \(Alpha w) (NonEmpty ws) ->
          let ws' = map getAlpha ws
              ns = renumber (ws' <> [w])
              moved = foldl (flip moveRight) ns [length ns, length ns - 1 .. 0]
          in map (snd . parseName) moved `shouldBe` (w:ws')

      it "moveRight <n> times places head last" $ do
        property $ \(Alpha w) (NonEmpty ws) ->
          let ws' = map getAlpha ws
              ns = renumber (w:ws')
              moved = foldl (flip moveRight) ns [0 .. length ns]
          in map (snd . parseName) moved `shouldBe` (ws' <> [w])

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
