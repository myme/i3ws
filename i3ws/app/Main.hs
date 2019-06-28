module Main where

import I3 hiding (command)
import I3.IPC (Invoker)
import I3WS
import I3WS.Workspaces
import Options.Applicative

data Options = Options { _handler :: Invoker -> IO () -- ^ Sub-command handler
                       , _debug :: I3Debug -- ^ Debug level
                       }

data MoveDir = MoveLeft | MoveRight

toDebug :: Int -> I3Debug
toDebug 0 = I3DebugOff
toDebug 1 = I3DebugInfo
toDebug _ = I3DebugTrace

opts :: Parser Options
opts = Options
  <$> commandParser
  <*> (toDebug . length <$> many debugParser)
  where debugParser = flag' () (short 'd' <> long "debug" <> help "Enable debug output")
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
