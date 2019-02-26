module Test.I3.Workspaces where

import I3.Workspaces
import Test.Hspec
import Test.I3.Mock
import Test.QuickCheck hiding (output)

newtype Alpha = Alpha { getAlpha :: String } deriving Show

instance Arbitrary Alpha where
  arbitrary = Alpha <$> listOf (elements (['A'..'Z'] <> ['a'..'z']))

tests :: Spec
tests = do
  describe "I3.Workspaces" $ do
    describe "getWorkspaces" $ do
      it "get empty list of workspaces" $ do
        mock <- defaultMock
        ws <- getWorkspaces mock
        ws `shouldBe` []

    describe "createWorkspace" $ do
      it "creates a new workspace with number" $ do
        mock <- defaultMock
        createWorkspace mock "foo"
        ws <- getWorkspaces mock
        map name ws `shouldBe` ["1:foo"]

      it "creates multiple workspaces with increasing numbers" $ do
        mock <- defaultMock
        createWorkspace mock "foo"
        createWorkspace mock "bar"
        ws <- getWorkspaces mock
        map name ws `shouldBe` ["1:foo", "2:bar"]

    describe "move" $ do
      it "moveLeft moving leftmost is identity" $ do
        property $ \(NonEmpty ws) -> moveLeft 0 ws `shouldBe` ws

      it "moveRight moving rightmost is identity" $ do
        property $ \(NonEmpty ws) -> moveRight (length ws - 1) ws `shouldBe` ws

      it "moveRight then moveLeft is identity" $ do
        property $ \(NonEmpty ws) -> moveLeft 1 (moveRight 0 ws) `shouldBe` ws

      it "moveLeft then moveRight is identity" $ do
        property $ \(NonEmpty ws) ->
          let last' = length ws
          in moveRight (last' - 1) (moveLeft last' ws) `shouldBe` ws

      it "moveLeft <n> times places last first" $ do
        property $ \(Alpha w) (NonEmpty ws) ->
          let ws' = map getAlpha ws
              moved = foldl (flip moveLeft) (ws' <> [w]) [length ws', length ws' - 1 .. 0]
          in map (snd . parseName) moved `shouldBe` (w:ws')

      it "moveRight <n> times places head last" $ do
        property $ \(Alpha w) (NonEmpty ws) ->
          let ws' = map getAlpha ws
              moved = foldl (flip moveRight) (w:ws') [0 .. length ws']
          in map (snd . parseName) moved `shouldBe` (ws' <> [w])

    describe "renumber" $ do
      it "is unchanged with sequenced numbers" $ do
        renumber ["1", "2", "3"] `shouldBe` ["1", "2", "3"]

      it "reorders unsorted numbers" $ do
        renumber ["2", "3", "1"] `shouldBe` ["1", "2", "3"]

      it "only changes number" $ do
        renumber ["2:foo", "3:bar", "1:baz"] `shouldBe` ["1:foo", "2:bar", "3:baz"]

      it "adds number to unnumbered workspaces" $ do
        renumber ["4:foo", "bar", "baz"] `shouldBe` ["1:foo", "2:bar", "3:baz"]

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