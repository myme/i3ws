module I3.Command where

import           Control.Exception
import           Data.Aeson (Value, withArray)
import           Data.ByteString.Lazy.UTF8 (fromString)
import qualified Data.Vector as V
import           I3.IPC

command :: Invoker -> String -> IO ()
command inv cmd = do
  res <- checkArraySuccess <$> invoke inv (Request Command (fromString cmd))
  either (throwIO . CommandFailed) pure res

checkArraySuccess :: Value -> Either String ()
checkArraySuccess = runParser $ withArray "[response]" $ \arr ->
  if length arr /= 1
    then pure (Left $ "Invalid number of responses: " <> show (length arr))
    else checkSuccess (V.head arr)
