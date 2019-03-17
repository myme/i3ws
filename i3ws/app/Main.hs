{-# LANGUAGE LambdaCase #-}

module Main where

import I3 hiding (command)
import I3WS
import I3WS.Workspaces
import System.Environment

usage :: IO ()
usage = putStrLn "i3ws [ monitor | move <dir> ]"

main :: IO ()
main = do
  i3 <- initI3
  getArgs >>= \case
    ["monitor"]       -> autoRenameWorkspaces i3
    ["move", "left"]  -> moveLeft i3
    ["move", "right"] -> moveRight i3
    _                 -> usage >> fail "Invalid arguments"
