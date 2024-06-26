module I3WS where

import Control.Arrow ((>>>))
import Control.Monad (unless, (<=<))
import Data.Aeson (Result (..), Value (..), withEmbeddedJSON, withObject, (.!=), (.:), (.:?))
import Data.Aeson.Types (parse)
import Data.Char (toLower)
import Data.IORef
import Data.Maybe (fromMaybe, mapMaybe)
import FontAwesome.Icons (icon)
import qualified FontAwesome.Icons as Icons
import I3
import I3.IPC
import I3.Tree hiding (Workspace)
import I3.Workspaces hiding (Workspace)
import I3WS.Types
import I3WS.Workspaces hiding (parse)

-- | Map window class to icons.
-- TODO: Make this configurable using a config file: ~/.config/i3ws.json
appIcon :: String -> String
appIcon app = case map toLower app of
  "bitwarden"            -> icon Icons.Shield
  "chromium-browser"     -> icon Icons.Chrome
  "cisco anyconnect secure mobility client"
                         -> icon Icons.Lock
  "emacs"                -> icon Icons.Code
  "firefox"              -> icon Icons.Firefox
  "gnome-control-center" -> icon Icons.Cog
  "alacritty"            -> icon Icons.Terminal
  "konsole"              -> icon Icons.Terminal
  "gnome-terminal"       -> icon Icons.Terminal
  "urxvt"                -> icon Icons.Terminal
  "xterm-256color"       -> icon Icons.Terminal
  "nautilus"             -> icon Icons.FolderOpen
  "qutebrowser"          -> icon Icons.Compass
  "slack"                -> icon Icons.Slack
  "spotify"              -> icon Icons.Spotify
  "systemsettings"       -> icon Icons.Cog
  _                      -> icon Icons.WindowMaximize

-- | Find suitable icons for workspace applications.
workspaceIcons :: Node -> String
workspaceIcons =
  leaves >>>
  map (win_class <=< node_window_properties) >>>
  map (appIcon . fromMaybe "") >>>
  unwords

-- | Add number and annotations to workspaces.
numberAndAnnotate :: Bool -> I3WS ()
numberAndAnnotate noRenumber = do
  inv <- i3ws_invoker <$> ask
  wss <- workspaces <$> getTree inv
  icons <- i3ws_icons <$> ask
  separator <- i3ws_separator <$> ask
  let (oldNames, newIcons) = unzip (mapMaybe wsIcons wss)
      renames
        | noRenumber = zipWith (changeLabel separator) oldNames newIcons
        | icons = renumber separator newIcons
        | otherwise = map show [1 :: Int ..]
  renameAll inv (zip oldNames renames)
  where
    wsIcons ws = do
      name' <- node_name ws
      pure (name', workspaceIcons ws)

-- TODO: Ensure tree isn't fetched when not needed (e.g. focus switch)
-- | Rename workspaces automatically based on contained windows.
autoRenameWorkspaces :: Bool -> I3WS ()
autoRenameWorkspaces noRenumber = do
  ignoreEvents <- liftIO (newIORef False)
  inv <- i3ws_invoker <$> ask
  numberAndAnnotate noRenumber
  subscribe inv [Window, Workspace, ETick] $ \case
    (ETick, payload) -> case parseTick payload of
      Error err        -> liftIO $ print err
      Success Nothing  -> liftIO $ pure ()
      Success (Just i) -> liftIO $ writeIORef ignoreEvents i
    _ -> do
      shouldIgnore <- liftIO (readIORef ignoreEvents)
      unless shouldIgnore (numberAndAnnotate noRenumber)
  where
    parseTick :: Value -> Result (Maybe Bool)
    parseTick = parse $ withObject "event" $ \event -> do
      first <- event .:? "first" .!= False
      if first
        then pure Nothing
        else Just <$> (
          event .: "payload" >>= withEmbeddedJSON "payload" (
              withObject "payload" (.: "ignoreEvents")))
