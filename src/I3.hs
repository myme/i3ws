module I3
  ( Event(..)
  , EventT(..)
  , command
  , initI3
  , getTree
  , getWorkspaces
  ) where

import Control.Monad (void)
import Data.ByteString.Lazy.Char8 (pack)
import I3.IPC hiding (ResponseT(Tree))
import I3.Internal (I3(..), i3CmdSocket, i3SocketPath, I3)
import I3.Tree (getTree)
import I3.Workspaces (getWorkspaces)

initI3 :: IO I3
initI3 = do
  socketPath <- getSocketPath
  cmdSock <- connect socketPath
  pure $ I3 { i3SocketPath = socketPath
            , i3CmdSocket = cmdSock
            }

command :: Invoker inv => inv -> String -> IO ()
command inv cmd = void $ invoke inv (Request RunCommand (pack cmd))
