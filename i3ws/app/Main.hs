module Main where

import I3

main :: IO ()
main = do
  i3 <- initI3
  print =<< getWorkspaces i3
