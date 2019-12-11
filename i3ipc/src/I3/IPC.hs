module I3.IPC where

import           Control.Monad
import           Control.Monad.Catch
import           Control.Monad.IO.Class
import           Data.Aeson (FromJSON, (.:), eitherDecode, encode, withObject)
import           Data.Aeson.Types
import           Data.Binary.Get
import           Data.Binary.Put
import           Data.Bits
import           Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy.Char8 as B8
import           Data.Char
import           I3.Internal
import           Network.Socket (Socket)
import qualified Network.Socket as Net
import qualified Network.Socket.ByteString.Lazy as NS
import           System.Process (readProcess)

data PackageType = Command
                 | Workspaces
                 | Subscribe
                 | Outputs
                 | Tree
                 | Marks
                 | BarConfig
                 | Version
                 | BindingModes
                 | Config
                 | Tick
                 | Sync
                 deriving (Bounded, Enum, Eq, Show)

data EventT = Workspace
            | Output
            | Mode
            | Window
            | BarConfigUpdate
            | Binding
            | Shutdown
            | ETick
            deriving (Bounded, Enum, Show)

data Request          = Request PackageType ByteString deriving Show
data Response     a   = Response PackageType (Either String a) deriving Show
data Event        a   = Event EventT (Either String a) deriving Show
type EventHandler m a = (MonadIO m) => (EventT, a) -> m ()

newtype I3Error = CommandFailed String deriving (Eq, Show)

instance Exception I3Error

data Invoker = Invoker
  { getInvoker    :: forall a m. (FromJSON a, Show a, MonadIO m) => Request -> m (Response a)
  , getSubscriber :: forall a m. (FromJSON a, Show a, MonadIO m, MonadMask m, MonadThrow m) => [EventT] -> EventHandler m a -> m ()
  }

i3Invoker :: I3 -> Invoker
i3Invoker i3 = Invoker
  { getInvoker = invoke' (i3Debug i3) (i3CmdSocket i3)
  , getSubscriber = subscribe' (i3Debug i3) (i3SocketPath i3)
  }

invoke :: (FromJSON a, Show a, MonadIO m, MonadThrow m) => Invoker -> Request -> m a
invoke inv req = do
  res <- getInvoker inv req
  let (Request  reqT _) = req
      (Response resT response) = res
  when (reqT /= resT) $
    throwM (CommandFailed "Mismatching request/response types")
  either (throwM . CommandFailed) pure response

subscribe :: (FromJSON a, Show a, MonadIO m, MonadMask m, MonadThrow m) => Invoker -> [EventT] -> EventHandler m a -> m ()
subscribe = getSubscriber

invoke' :: (FromJSON a, Show a, MonadIO m) => I3Debug -> Socket -> Request -> m (Response a)
invoke' debug sock req = do
  when (debug >= I3DebugInfo) (liftIO $ print req)
  res <- liftIO $ send sock req >> recv sock
  when (debug >= I3DebugTrace) (liftIO $ print res)
  return res

eventString :: EventT -> String
eventString ETick = "tick"
eventString ev    = map toLower (show ev)

subscribe' :: (FromJSON a, Show a, MonadIO m, MonadMask m, MonadThrow m) => I3Debug -> FilePath -> [EventT] -> EventHandler m a -> m ()
subscribe' debug socketPath events handler =
  bracket (connect socketPath) close $ \sock -> do
    let req = Request Subscribe eventsJson
        eventsJson = encode $ map eventString events
    (Response _ res) <- invoke' debug sock req
    case res >>= runParser checkSuccess of
      Left err -> throwM (CommandFailed err)
      Right () -> handleEvents sock
  where handleEvents sock = do
          (Event type' payload) <- recvEvent sock debug
          case payload of
            Left err -> throwM (CommandFailed err)
            Right  x -> handler (type', x) >> handleEvents sock

runParser :: (Value -> Parser (Either String a)) -> Value -> Either String a
runParser parser = join . parseEither parser

checkSuccess :: Value -> Parser (Either String ())
checkSuccess = withObject "response" $ \obj -> do
  success <- obj .: "success"
  if not success
    then Left <$> (obj .: "error")
    else pure (Right ())

getSocketPath :: IO FilePath
getSocketPath =
  head . lines <$> readProcess "i3" ["--get-socketpath"] mempty

connect :: MonadIO m => FilePath -> m Socket
connect sockPath = do
  sock <- liftIO (Net.socket Net.AF_UNIX Net.Stream 0)
  liftIO $ Net.connect sock (Net.SockAddrUnix sockPath)
  pure sock

close :: MonadIO m => Socket -> m ()
close = liftIO . Net.close

magic :: ByteString
magic = "i3-ipc"

safeToEnum :: (Enum t, Bounded t) => Int -> Maybe t
safeToEnum i = let r = toEnum i
                   fromEnum' = fromEnum . asTypeOf r
               in if i >= fromEnum' minBound && i <= fromEnum' maxBound
               then Just r
               else Nothing

encodeMsg :: Request -> ByteString
encodeMsg (Request type' payload) = runPut $ do
  putLazyByteString magic
  putInt32host (fromIntegral $ B8.length payload)
  putInt32host (fromIntegral $ fromEnum type')
  putLazyByteString payload

send :: Socket -> Request -> IO ()
send sock req = do
  let package = encodeMsg req
  void $ NS.send sock package

decodeHeader :: ByteString -> (Either EventT PackageType, Int)
decodeHeader = runGet getHeader where
  getType t = case safeToEnum (fromIntegral t) of
    Nothing -> fail ("Invalid message type: " <> show t)
    Just t' -> pure t'
  getHeader = do
    m <- getLazyByteString (fromIntegral $ B8.length magic)
    when (m /= magic) $ fail ("Invalid magic: " <> show m)
    len <- getInt32host
    t <- getInt32host
    type' <- if testBit t 31
      then Left <$> getType (clearBit t 31)
      else Right <$> getType t
    pure (type', fromIntegral len)

recvPacket :: Socket -> IO (Either EventT PackageType, ByteString)
recvPacket sock = do
  let headerLength = B8.length magic + 4 + 4 -- Magic + 2x 32 bit ints
  (type', payloadLength) <- decodeHeader <$> NS.recv sock (fromIntegral headerLength)
  payload <- NS.recv sock (fromIntegral payloadLength)
  pure (type', payload)

recv :: FromJSON a => Socket -> IO (Response a)
recv sock = do
  (type', payload) <- recvPacket sock
  resType <- either (\_ -> fail "Unexpected Event") pure type'
  pure (Response resType (eitherDecode payload))

recvEvent :: (FromJSON a, MonadIO m) => Socket -> I3Debug -> m (Event a)
recvEvent sock debug = do
  (type', payload) <- liftIO (recvPacket sock)
  evType <- either pure (\_ -> fail "Expecting Event") type'
  when (debug >= I3DebugInfo) $ liftIO $
    putStrLn $ "Event " <> show evType <> " " <> show payload
  pure (Event evType (eitherDecode payload))
