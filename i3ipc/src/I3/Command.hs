module I3.Command where

import Data.ByteString.Lazy.Char8 (pack)
import I3.IPC

command :: Invoker -> String -> IO (Either String ())
command inv cmd = do
  res <- invoke inv (Request Command (pack cmd))
  pure (res >>= checkSuccess)
