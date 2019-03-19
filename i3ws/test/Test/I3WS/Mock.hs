{-# LANGUAGE OverloadedStrings #-}

module Test.I3WS.Mock where

import           Data.Aeson (decode, encode)
import           Data.ByteString.Lazy.Char8 (ByteString, split)
import           Data.Function
import           Data.IORef
import           Data.List
import qualified I3.IPC as IPC
import           I3.IPC hiding (Workspace)
import           I3.Workspaces
import           I3WS.Workspaces

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
  where cmp (Nothing, _) (Nothing, _) = LT
        cmp (Nothing, _) (Just _, _)  = GT
        cmp (Just _, _)  (Nothing, _) = LT
        cmp a b = compare a b

-- | Create a I3 mock backend
defaultMock :: IO Invoker
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

  pure (Invoker handlerWithLog undefined)

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
    let new = defaultWorkspace { name = name' }
        maybeAddNew wss = case find ((== name') . name) wss of
          Nothing -> wss <> [new]
          Just _  -> wss
        setFocus ws = ws { focused = name ws == name' }
    modifyIORef' ref (sortWs . map setFocus . maybeAddNew)
    pure (Response Command "")
