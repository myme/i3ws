module I3WS.Types
 ( I3WS
 , Config(..)
 , ask
 , liftIO
 , runI3WS
 ) where

import Control.Monad.IO.Class
import Control.Monad.Trans.Reader
import I3 hiding (command)
import I3.IPC

data Config = Config { i3ws_debug :: I3Debug
                     , i3ws_icons :: Bool
                     , i3ws_invoker :: Invoker
                     }

type I3WS = ReaderT Config IO

runI3WS :: Config -> I3WS a -> IO a
runI3WS = flip runReaderT
