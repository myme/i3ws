{-# LANGUAGE OverloadedStrings #-}

module Test.I3.Mock where

import           Data.Aeson (decode, encode)
import           Data.ByteString.Lazy.Char8 (ByteString, split)
import           Data.IORef
import qualified I3.IPC as IPC
import           I3.IPC hiding (Workspace)
import           I3.Workspaces

data MockI3 = MockI3
  { mockHandler :: Request -> IO Response
  , mockGetLog :: IO [String]
  }

instance Invoker MockI3 where
  invoke = mockHandler
  subscribe = undefined

getMockLog :: MockI3 -> IO [String]
getMockLog = mockGetLog

defaultWorkspace :: Workspace
defaultWorkspace = Workspace
  { num = -1
  , name = "workspace"
  , focused = True
  , visible = True
  , rect = Geometry 0 0 1280 1024
  , output = "DP-1"
  , urgent = False
  }

-- | Create a I3 mock backend
defaultMock :: IO MockI3
defaultMock = do
  workspaces <- newIORef mempty
  mockLog    <- newIORef mempty

  let handler (Request GetWorkspaces _) = do
        ws <- readIORef workspaces
        pure (Response IPC.Workspaces (encode ws))
      handler (Request RunCommand cmd) = case split ' ' cmd of
        ["workspace", n] -> switchWorkspace workspaces n
        ["rename", "workspace", from, "to", to] -> renameWorkspace workspaces from to
        xs -> print ("Unknown command: " : xs) >> undefined
      handler _ = undefined

  let handlerWithLog req = do
        modifyIORef' mockLog (<> [show req])
        handler req

  pure (MockI3 handlerWithLog (readIORef mockLog))

renameWorkspace :: IORef [Workspace] -> ByteString -> ByteString -> IO Response
renameWorkspace ref from to = case (,) <$> decode from <*> decode to of
  Nothing -> pure (Response Command "{\"success\":false}")
  Just (f, t) -> do
    let rename' ws | name ws /= f = ws
                   | otherwise = ws { name = t }
    modifyIORef' ref (map rename')
    pure (Response Command "")

switchWorkspace :: IORef [Workspace] -> ByteString -> IO Response
switchWorkspace ref n = case decode n of
  Nothing -> pure (Response Command "{\"success\":false}")
  Just name' -> do
    let ws = defaultWorkspace { name = name' }
    modifyIORef' ref (<> [ws])
    pure (Response Command "")
