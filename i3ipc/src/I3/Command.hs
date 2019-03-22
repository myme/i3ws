module I3.Command where

import           Control.Exception
import           Control.Monad
import           Data.Aeson (Value, (.:), withArray, withObject)
import           Data.Aeson.Types
import           Data.ByteString.Lazy.UTF8 (fromString)
import qualified Data.Vector as V
import           I3.IPC hiding (checkSuccess)

command :: Invoker -> String -> IO ()
command inv cmd = do
  res <- checkSuccess <$> invoke inv (Request Command (fromString cmd))
  either (throwIO . CommandFailed) pure res

checkSuccess :: Value -> Either String ()
checkSuccess = join . parseEither (withArray "[response]" $ parseResponse . V.head)
  where parseResponse = withObject "response" $ \obj -> do
          success <- obj .: "success"
          if not success
            then Left <$> (obj .: "error")
            else pure (Right ())
