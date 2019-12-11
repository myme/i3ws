module Main where

import I3 hiding (command)
import I3WS
import I3WS.Types
import I3WS.Workspaces
import Options.Applicative

data Options = Options { _command :: Command -- ^ Sub-command handler
                       , _debug :: I3Debug -- ^ Debug level
                       , _icons :: Bool -- ^ Use FontAwesome icons in workspace names
                       , _separator :: String -- ^ Separator between workspace number and icons
                       }

data MoveDir = MoveLeft | MoveRight
data Command = Monitor | Move MoveDir | New

toDebug :: Int -> I3Debug
toDebug 0 = I3DebugOff
toDebug 1 = I3DebugInfo
toDebug _ = I3DebugTrace

opts :: Parser Options
opts = Options
  <$> commandParser
  <*> (toDebug . length <$> many debugParser)
  <*> iconsParser
  <*> separatorParser
  where debugParser = flag' () (short 'd' <> long "debug" <> help "Enable debug output")
        iconsParser = flag False True (short 'i' <> long "icons" <> help "Use FontAwesome icons")
        separatorParser = option str (short 's' <> long "separator" <> help "Separator between number and icons")
        readDir "left"  = Just MoveLeft
        readDir "right" = Just MoveRight
        readDir _ = Nothing
        commandParser = subparser (
          command "monitor" (
              info (pure Monitor) (progDesc "Automatically name i3 workspaces")) <>
          command "move" (
              info (Move <$> argument (maybeReader readDir) idm) (progDesc "Move a workspace")) <>
          command "new" (
              info (pure New) (progDesc "Create a new workspace")))

-- TODO: Add command to set workspace icon/text Default is to add icons based on
-- the workspace apps, but would be nice to allow setting text and icons based
-- on semantics too.
main :: IO ()
main = do
  options <- execParser $
    info (opts <**> helper) (fullDesc <> progDesc "Various handy i3 integrations")
  i3 <- initI3 (_debug options)
  let config = Config
        { i3ws_debug = _debug options
        , i3ws_icons = _icons options
        , i3ws_separator = _separator options
        , i3ws_invoker = i3
        }
  runI3WS config $ case _command options of
    Monitor -> autoRenameWorkspaces
    Move MoveLeft -> moveLeft
    Move MoveRight -> moveRight
    New -> newWorkspace
