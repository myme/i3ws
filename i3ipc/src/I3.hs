module I3
  ( Event(..)
  , EventT(..)
  , I3Debug(..)
  , command
  , initI3
  , getTree
  , getWorkspaces
  ) where

import Control.Monad.Catch
import Control.Monad.IO.Class
import I3.Command (command)
import I3.IPC
import I3.Internal (i3CmdSocket, i3SocketPath, I3(..), I3Debug(..))
import I3.Tree (getTree)
import I3.Workspaces (getWorkspaces)

initI3 :: (MonadIO m, MonadMask m) => I3Debug -> IO (Invoker m)
initI3 debug = do
  socketPath <- getSocketPath
  cmdSock <- connect socketPath
  pure $ i3Invoker $ I3
    { i3SocketPath = socketPath
    , i3CmdSocket = cmdSock
    , i3Debug = debug
    }
