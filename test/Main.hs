module Main where

import           Test.Hspec
import qualified Test.I3.Workspaces as Workspaces

main :: IO ()
main = hspec $ do
  Workspaces.tests
