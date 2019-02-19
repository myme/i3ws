module Main where

import Control.Monad (forM_)
import I3

-- workspaceNumber :: String ->

main :: IO ()
main = do
  i3 <- initI3
  workspaces <- getWorkspaces i3
  forM_ workspaces print
  -- command i3 "exec xcalc"
  -- subscribeEvents i3 [Window] $ \ev -> do
  --   let (Event _ payload) = ev
  --   print ev
  --   case decode payload >>= parseMaybe (.: "change") of
  --     Nothing     -> putStrLn "No event 'change'"
  --     Just change -> putStrLn change
