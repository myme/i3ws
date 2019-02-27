module Main where

import I3
import I3.Workspaces

main :: IO ()
main = do
  i3 <- initI3

  -- Tree
  -- print =<< getTree i3

  -- Workspaces
  -- workspaces <- getWorkspaces i3
  -- forM_ workspaces print
  assignNumbers i3
  -- moveFocusedRight i3

  -- Commands
  -- command i3 "exec xcalc"
  -- subscribeEvents i3 [Window] $ \ev -> do
  --   let (Event _ payload) = ev
  --   print ev
  --   case decode payload >>= parseMaybe (.: "change") of
  --     Nothing     -> putStrLn "No event 'change'"
  --     Just change -> putStrLn change
