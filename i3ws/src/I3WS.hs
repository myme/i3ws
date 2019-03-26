module I3WS where

import Control.Arrow ((>>>))
import Control.Monad (unless)
import Data.IORef
import Data.Aeson ((.:), (.:?), (.!=), withObject, withEmbeddedJSON, Result(..), Value(..))
import Data.Aeson.Types (parse)
import Data.Char (toLower)
import Data.Maybe (mapMaybe, fromMaybe)
import FontAwesome.Icons
import I3
import I3.IPC
import I3.Tree hiding (Workspace)
import I3.Workspaces hiding (Workspace)
import I3WS.Workspaces hiding (parse)

-- | Map window class to icons.
appIcon :: String -> String
appIcon app = case map toLower app of
  "bitwarden"            -> icon Shield
  "chromium-browser"     -> icon Chrome
  "cisco anyconnect secure mobility client"
                         -> icon Lock
  "emacs"                -> icon Code
  "firefox"              -> icon Firefox
  "gnome-control-center" -> icon Cog
  "gnome-terminal"       -> icon Terminal
  "nautilus"             -> icon FolderOpen
  "qutebrowser"          -> icon Compass
  "spotify"              -> icon Spotify
  _                      -> icon WindowMaximize

-- | Find suitable icons for workspace applications.
workspaceIcons :: Node -> String
workspaceIcons =
  leaves >>>
  map (fmap win_class . node_window_properties) >>>
  map (appIcon . fromMaybe "") >>>
  unwords

-- | Add number and annotations to workspaces.
numberAndAnnotate :: Invoker -> IO ()
numberAndAnnotate inv = do
  wss <- workspaces <$> getTree inv
  let (oldNames, withIcons) = unzip (mapMaybe wsIcons wss)
      renames = zip oldNames (renumber withIcons)
  renameAll inv renames
  where wsIcons ws = do
          name' <- node_name ws
          pure (name', workspaceIcons ws)

-- | Rename workspaces automatically based on contained windows.
autoRenameWorkspaces :: Invoker -> IO ()
autoRenameWorkspaces inv = do
  ignoreEvents <- newIORef False
  numberAndAnnotate inv
  subscribe inv [Window, Workspace, ETick] $ \case
    (ETick, payload) -> do
      case parseTick payload of
        Error err        -> print err
        Success Nothing  -> pure ()
        Success (Just i) -> writeIORef ignoreEvents i
      readIORef ignoreEvents >>= print
    _ -> do
      shouldIgnore <- readIORef ignoreEvents
      unless shouldIgnore (numberAndAnnotate inv)
  where
    parseTick :: Value -> Result (Maybe Bool)
    parseTick = parse $ withObject "event" $ \event -> do
      first <- event .:? "first" .!= False
      if first
        then pure Nothing
        else Just <$> (
          event .: "payload" >>= withEmbeddedJSON "payload" (
              withObject "payload" (.: "ignoreEvents")))
