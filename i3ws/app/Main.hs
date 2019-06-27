module Main where

import I3 hiding (command)
import I3.IPC (Invoker)
import I3WS
import I3WS.Workspaces
import Options.Applicative

data Options = Options { _handler :: Invoker -> IO ()
                       , _debug :: Bool
                       }

data MoveDir = MoveLeft | MoveRight

opts :: Parser Options
opts = Options <$> commandParser <*> debugParser
  where debugParser = flag False True (short 'd' <> long "debug" <> help "Enable debug output")
        move MoveLeft  = moveLeft
        move MoveRight = moveRight
        readDir "left"  = Just MoveLeft
        readDir "right" = Just MoveRight
        readDir _ = Nothing
        commandParser = subparser (
          command "monitor" (
              info (pure autoRenameWorkspaces) (progDesc "Automatically name i3 workspaces")) <>
          command "move" (
              info (move <$> argument (maybeReader readDir) idm) (progDesc "Move a workspace")) <>
          command "new" (
              info (pure newWorkspace) (progDesc "Create a new workspace")))

main :: IO ()
main = do
  options <- execParser $
    info (opts <**> helper) (fullDesc <> progDesc "Various handy i3 integrations")
  i3 <- initI3 (_debug options)
  _handler options i3
