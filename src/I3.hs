module I3
  ( initI3
  , command
  , subscribeEvents
  , EventT(..)
  , Event(..)
  , getWorkspaces
  ) where

import           Control.Exception (bracket)
import           Control.Monad (unless, forever, void)
import           Data.Aeson (decode)
import           Data.ByteString.Lazy.Char8 (pack)
import qualified Data.Map.Strict as Map
import           Data.Maybe
import           I3.IPC
import           I3.Internal
import           I3.Workspaces

type EventHandler = Event -> IO ()

initI3 :: IO I3
initI3 = do
  socketPath <- getSocketPath
  cmdSock <- connect socketPath
  pure $ I3 { i3SocketPath = socketPath
            , i3CmdSocket = cmdSock
            }

command :: I3 -> String -> IO ()
command i3 cmd = do
  let sock = i3CmdSocket i3
  void $ invoke sock (Request RunCommand (pack cmd))

subscribeEvents :: I3 -> [EventT] -> EventHandler -> IO ()
subscribeEvents i3 events handler = do
  let socketPath = i3SocketPath i3
  bracket (connect socketPath) close $ \sock -> do
    (Response _ payload) <- invoke sock (subscribe events)
    let success = fromMaybe False (decode payload >>= Map.lookup "success")
    unless success $ fail "Event subscription failed!"
    forever (recvEvent sock >>= handler)
