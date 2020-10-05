module I3WS where

import Control.Arrow ((>>>))
import Control.Monad ((<=<), unless)
import Data.Aeson ((.:), (.:?), (.!=), withObject, withEmbeddedJSON, Result(..), Value(..))
import Data.Aeson.Types (parse)
import Data.Char (toLower)
import Data.IORef
import Data.Maybe (mapMaybe, fromMaybe)
import FontAwesome.Icons
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
  "bitwarden"            -> icon Shield
  "chromium-browser"     -> icon Chrome
  "cisco anyconnect secure mobility client"
                         -> icon Lock
  "emacs"                -> icon Code
  "firefox"              -> icon Firefox
  "gnome-control-center" -> icon Cog
  "alacritty"            -> icon Terminal
  "konsole"              -> icon Terminal
  "gnome-terminal"       -> icon Terminal
  "urxvt"                -> icon Terminal
  "xterm-256color"       -> icon Terminal
  "nautilus"             -> icon FolderOpen
  "qutebrowser"          -> icon Compass
  "spotify"              -> icon Spotify
  _                      -> icon WindowMaximize

-- | Find suitable icons for workspace applications.
workspaceIcons :: Node -> String
workspaceIcons =
  leaves >>>
  map (win_class <=< node_window_properties) >>>
  map (appIcon . fromMaybe "") >>>
  unwords

-- | Add number and annotations to workspaces.
numberAndAnnotate :: I3WS ()
numberAndAnnotate = do
  inv <- i3ws_invoker <$> ask
  wss <- workspaces <$> getTree inv
  icons <- i3ws_icons <$> ask
  separator <- i3ws_separator <$> ask
  let (oldNames, withIcons) = unzip (mapMaybe wsIcons wss)
      renumbered = if icons then renumber separator withIcons else map show [1 :: Int ..]
      renames = zip oldNames renumbered
  renameAll inv renames
  where wsIcons ws = do
          name' <- node_name ws
          pure (name', workspaceIcons ws)

-- TODO: Ensure tree isn't fetched when not needed (e.g. focus switch)
-- | Rename workspaces automatically based on contained windows.
autoRenameWorkspaces :: I3WS ()
autoRenameWorkspaces = do
  ignoreEvents <- liftIO (newIORef False)
  inv <- i3ws_invoker <$> ask
  numberAndAnnotate
  subscribe inv [Window, Workspace, ETick] $ \case
    (ETick, payload) -> case parseTick payload of
      Error err        -> liftIO $ print err
      Success Nothing  -> liftIO $ pure ()
      Success (Just i) -> liftIO $ writeIORef ignoreEvents i
    _ -> do
      shouldIgnore <- liftIO (readIORef ignoreEvents)
      unless shouldIgnore numberAndAnnotate
  where
    parseTick :: Value -> Result (Maybe Bool)
    parseTick = parse $ withObject "event" $ \event -> do
      first <- event .:? "first" .!= False
      if first
        then pure Nothing
        else Just <$> (
          event .: "payload" >>= withEmbeddedJSON "payload" (
              withObject "payload" (.: "ignoreEvents")))
