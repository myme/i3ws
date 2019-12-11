module Test.I3WS.Mock where

import           Data.Aeson (FromJSON, decode, eitherDecode, encode)
import           Data.ByteString.Lazy.Char8 (ByteString, split)
import           Data.Function
import           Data.IORef
import           Data.List
import           I3 (I3Debug(..))
import qualified I3.IPC as IPC
import           I3.IPC hiding (Config, Workspace)
import           I3.Workspaces
import           I3WS.Types
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


-- | Run an action in a mock context
runMock :: (Invoker I3WS -> I3WS a) -> IO a
runMock action = do
  mock <- defaultMock
  runI3WS (mkconfig mock) (action mock)
  where mkconfig inv = Config
          { i3ws_debug = I3DebugOff
          , i3ws_icons = True
          , i3ws_separator = ":"
          , i3ws_invoker = inv
          }


-- | Create a I3 mock backend
defaultMock :: IO (Invoker I3WS)
defaultMock = do
  workspaces <- newIORef mempty
  mockLog    <- newIORef mempty

  let handler :: FromJSON a => Request -> I3WS (Response a)
      handler (Request Tick _) = pure (Response Tick (eitherDecode "{\"success\":true}"))
      handler (Request Workspaces _) = do
        ws <- liftIO (readIORef workspaces)
        pure (Response IPC.Workspaces (eitherDecode (encode ws)))
      handler (Request Command cmd) = case split ' ' cmd of
        ["workspace", n] -> switchWorkspace workspaces n
        ["rename", "workspace", from, "to", to] -> renameWorkspace workspaces from to
        xs -> liftIO (print ("Unknown command: " : xs)) >> undefined
      handler _ = undefined

  let handlerWithLog req = do
        liftIO $ modifyIORef' mockLog (<> [show req])
        handler req

  pure (Invoker handlerWithLog undefined)

renameWorkspace :: FromJSON a => IORef Workspaces -> ByteString -> ByteString -> I3WS (Response a)
renameWorkspace ref from to = case (,) <$> decode from <*> decode to of
  Nothing -> pure (Response Command (eitherDecode "[{\"success\":false,\"error\":\"foo\"}]"))
  Just (f, t) -> do
    let rename' ws | name ws == f = ws { name = t }
                   | otherwise = ws
    liftIO $ modifyIORef' ref (sortWs . map rename')
    pure (Response Command (eitherDecode "[{\"success\":true}]"))

switchWorkspace :: FromJSON a => IORef Workspaces -> ByteString -> I3WS (Response a)
switchWorkspace ref n = case decode n of
  Nothing -> pure (Response Command (eitherDecode "[{\"success\":false,\"error\":\"foo\"}]"))
  Just name' -> do
    let new = defaultWorkspace { name = name' }
        maybeAddNew wss = case find ((== name') . name) wss of
          Nothing -> wss <> [new]
          Just _  -> wss
        setFocus ws = ws { focused = name ws == name' }
    liftIO $ modifyIORef' ref (sortWs . map setFocus . maybeAddNew)
    pure (Response Command (eitherDecode "[{\"success\":true}]"))
