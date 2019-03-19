module I3
  ( Event(..)
  , EventT(..)
  , command
  , initI3
  , getTree
  , getWorkspaces
  ) where

import I3.Command (command)
import I3.IPC hiding (ResponseT(Tree))
import I3.Internal (i3CmdSocket, i3SocketPath, I3(..))
import I3.Tree (getTree)
import I3.Workspaces (getWorkspaces)

initI3 :: IO Invoker
initI3 = do
  socketPath <- getSocketPath
  cmdSock <- connect socketPath
  pure $ i3Invoker $ I3
    { i3SocketPath = socketPath
    , i3CmdSocket = cmdSock
    , i3Trace = False
    }
