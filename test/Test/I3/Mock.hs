{-# LANGUAGE OverloadedStrings #-}

module Test.I3.Mock where

import           Data.Aeson (decode, encode)
import           Data.ByteString.Lazy.Char8 (ByteString, split)
import           Data.Function
import           Data.IORef
import           Data.List
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
  , focused = False
  , visible = False
  , rect = Geometry 0 0 1280 1024
  , output = "DP-1"
  , urgent = False
  }

type Workspaces = [Workspace]

sortWs :: Workspaces -> Workspaces
sortWs = sortBy (cmp `on` parseName . name)
  where cmp (Nothing, a) (Nothing, b) = compare a b
        cmp (Nothing, _) (Just _, _)  = GT
        cmp (Just _, _)  (Nothing, _) = LT
        cmp a b = compare a b

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

renameWorkspace :: IORef Workspaces -> ByteString -> ByteString -> IO Response
renameWorkspace ref from to = case (,) <$> decode from <*> decode to of
  Nothing -> pure (Response Command "{\"success\":false}")
  Just (f, t) -> do
    let rename' ws | name ws == f = ws { name = t }
                   | otherwise = ws
    modifyIORef' ref (sortWs . map rename')
    pure (Response Command "")

switchWorkspace :: IORef Workspaces -> ByteString -> IO Response
switchWorkspace ref n = case decode n of
  Nothing -> pure (Response Command "{\"success\":false}")
  Just name' -> do
    let new = defaultWorkspace { name = name', focused = True }
    let blur ws = ws { focused = False }
    modifyIORef' ref (sortWs . (<> [new]) . map blur)
    pure (Response Command "")
