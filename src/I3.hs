module I3
  ( initI3
  , initI3Subscribe
  , command
  , getWorkspaces
  ) where

import Control.Monad (void)
import Data.ByteString.Lazy.Char8 (pack, unpack)
import I3.IPC
import Network.Socket (Socket)

type EventHandler = Event -> I3 -> IO ()

data I3 = I3
  { i3CmdSocket :: Socket
  , i3EventSocket :: Maybe Socket
  , i3EventHandler :: Maybe EventHandler
  }

initI3 :: IO I3
initI3 = do
  cmdSock <- connect
  pure $ I3 { i3CmdSocket = cmdSock
            , i3EventSocket = Nothing
            , i3EventHandler = Nothing
            }

initI3Subscribe :: [EventT] -> EventHandler -> IO I3
initI3Subscribe events handler = do
  cmdSock <- connect
  evSock  <- connect
  res <- invoke evSock (subscribe events)
  print res
  pure $ I3 { i3CmdSocket = cmdSock
            , i3EventSocket = Just evSock
            , i3EventHandler = Just handler
            }

command :: I3 -> String -> IO ()
command i3 cmd = do
  let sock = i3CmdSocket i3
  void $ invoke sock (Request RunCommand (pack cmd))

getWorkspaces :: I3 -> IO String
getWorkspaces i3 = do
  let sock = i3CmdSocket i3
  (Response _ payload) <- invoke sock (Request GetWorkspaces mempty)
  pure $ unpack payload
