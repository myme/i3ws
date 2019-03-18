{-# LANGUAGE LambdaCase #-}

module Main where

import I3 hiding (command)
import I3WS
import I3WS.Workspaces
import System.Environment

usage :: IO ()
usage = do
  putStrLn "usage: i3ws <action>"
  putStrLn ""
  putStrLn "actions:"
  putStrLn ""
  putStrLn "  monitor                  update workspace names on changes"
  putStrLn "  move [left|right|new]    move/reorder workspace or container"
  putStrLn "  new                      create a new workspace"
  putStrLn ""

main :: IO ()
main = do
  i3 <- initI3
  getArgs >>= \case
    ["monitor"]       -> autoRenameWorkspaces i3
    ["move", "left"]  -> moveLeft i3
    ["move", "right"] -> moveRight i3
    ["move", "new"]   -> moveNew i3
    ["new"]           -> newWorkspace i3
    _                 -> usage >> fail "Invalid arguments"
