{-# LANGUAGE LambdaCase #-}

module Main where

import I3 hiding (command)
import I3WS
import System.Environment

monitor :: IO ()
monitor = do
  i3 <- initI3
  autoRenameWorkspaces i3

data Direction = Left' | Right'

move :: Direction -> IO ()
move _ = fail "move not implemented"

usage :: IO ()
usage = putStrLn "i3ws [ monitor | move <dir> ]"

main :: IO ()
main = getArgs >>= \case
  ["monitor"]       -> monitor
  ["move", "left"]  -> move Left'
  ["move", "right"] -> move Right'
  _                 -> usage >> fail "Invalid arguments"
