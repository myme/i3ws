module I3.Command where

import Control.Monad (void)
import Data.ByteString.Lazy.Char8 (pack)
import I3.IPC

command :: Invoker -> String -> IO ()
command inv cmd = void $ invoke inv (Request RunCommand (pack cmd))
