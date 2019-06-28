module I3.Internal where

import Network.Socket (Socket)

data I3Debug = I3DebugOff | I3DebugInfo | I3DebugTrace
  deriving (Eq, Ord)

data I3 = I3
  { i3SocketPath :: FilePath
  , i3CmdSocket :: Socket
  , i3Trace :: I3Debug
  }
