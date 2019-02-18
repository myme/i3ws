module Main where

import I3

main :: IO ()
main = do
  i3 <- initI3
  print =<< getWorkspaces i3
  command i3 "exec xcalc"
  subscribeEvents i3 [Window, Workspace] print
