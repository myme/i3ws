module I3.Internal where

import Network.Socket (Socket)

data I3Debug = I3DebugOff | I3DebugInfo | I3DebugTrace
  deriving (Eq, Ord, Show)

data I3 = I3
  { i3SocketPath :: FilePath
  , i3CmdSocket :: Socket
  , i3Debug :: I3Debug
  }
