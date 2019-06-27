module Main where

import Control.Monad
import I3 hiding (command)
import I3.IPC (Invoker)
import I3WS
import I3WS.Workspaces
import Options.Applicative

data Dir = MoveLeft | MoveRight

opts :: Invoker -> Parser (IO ())
opts i3 = subparser
  (  command "monitor" (info (pure monitor) idm)
  <> command "move" (info (move <$> argument (maybeReader readDir) idm) idm)
  <> command "new" (info (pure new) idm) )
  where monitor = autoRenameWorkspaces i3
        move MoveLeft  = moveLeft i3
        move MoveRight = moveRight i3
        new = newWorkspace i3
        readDir "left"  = Just MoveLeft
        readDir "right" = Just MoveRight
        readDir _ = Nothing

main :: IO ()
main = do
  i3 <- initI3
  join $ execParser (info (opts i3) idm)
