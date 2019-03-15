module I3WS where

import Control.Arrow ((>>>))
import Data.Char (toLower)
import Data.Maybe (mapMaybe, fromMaybe)
import FontAwesome.Icons
import I3
import I3.IPC
import I3.Tree hiding (Workspace)
import I3.Workspaces hiding (Workspace)
import I3WS.Workspaces

-- | Map window class to icons.
appIcon :: String -> String
appIcon app = case map toLower app of
  "bitwarden"            -> icon "shield"
  "chromium-browser"     -> icon "chrome"
  "cisco anyconnect secure mobility client"
                         -> icon "lock"
  "emacs"                -> icon "code"
  "firefox"              -> icon "firefox"
  "gnome-control-center" -> icon "cog"
  "gnome-terminal"       -> icon "terminal"
  "nautilus"             -> icon "folder-open"
  "qutebrowser"          -> icon "compass"
  "spotify"              -> icon "spotify"
  _                      -> icon "window-maximize"
  where icon i = maybe i pure (lookup i icons)

-- | Find suitable icons for workspace applications.
workspaceIcons :: Node -> String
workspaceIcons =
  leaves >>>
  map (fmap win_class . node_window_properties) >>>
  map (appIcon . fromMaybe "") >>>
  unwords

-- | Find active workspaces.
activeWorkspaces :: Invoker inv => inv -> IO [Node]
activeWorkspaces = fmap nonEmpty . getTree
  where nonEmpty = filter (not . null . leaves) . workspaces

-- | Add number and annotations to workspaces.
numberAndAnnotate :: Invoker inv => inv -> IO ()
numberAndAnnotate inv = do
  wss <- activeWorkspaces inv
  let (oldNames, withIcons) = unzip (mapMaybe wsIcons wss)
      renames = zip oldNames (renumber withIcons)
  renameAll inv renames
  where wsIcons ws = do
          name' <- node_name ws
          pure (name', workspaceIcons ws)

-- | Rename workspaces automatically based on contained windows.
autoRenameWorkspaces :: Invoker inv => inv -> IO ()
autoRenameWorkspaces inv = do
  let handler = numberAndAnnotate inv
  handler
  subscribe inv [Window, Workspace] (const handler)

run :: IO ()
run = do
  i3 <- initI3
  autoRenameWorkspaces i3
