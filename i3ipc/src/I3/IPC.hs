{-# LANGUAGE RankNTypes #-}

module I3.IPC where

import           Control.Exception
import           Control.Monad
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

data Request        = Request PackageType ByteString deriving Show
data Response     a = Response PackageType (Either String a) deriving Show
data Event        a = Event EventT (Either String a) deriving Show
type EventHandler a = (EventT, a) -> IO ()

newtype I3Error = CommandFailed String deriving (Eq, Show)

instance Exception I3Error

data Invoker = Invoker
  { getInvoker    :: forall a. (FromJSON a, Show a) => Request -> IO (Response a)
  , getSubscriber :: forall a. FromJSON a => [EventT] -> EventHandler a -> IO ()
  }

i3Invoker :: I3 -> Invoker
i3Invoker i3 = Invoker
  { getInvoker = invoke' (i3Trace i3) (i3CmdSocket i3)
  , getSubscriber = subscribe' (i3Trace i3) (i3SocketPath i3)
  }

invoke :: (FromJSON a, Show a) => Invoker -> Request -> IO a
invoke inv req = do
  res <- getInvoker inv req
  let (Request  reqT _) = req
      (Response resT response) = res
  when (reqT /= resT) $
    throwIO (CommandFailed "Mismatching request/response types")
  either (throwIO . CommandFailed) pure response

subscribe :: FromJSON a => Invoker -> [EventT] -> EventHandler a -> IO ()
subscribe = getSubscriber

invoke' :: (FromJSON a, Show a) => Bool -> Socket -> Request -> IO (Response a)
invoke' trace sock req = do
  when trace (print req)
  res <- send sock req >> recv sock
  when trace (print res)
  return res

eventString :: EventT -> String
eventString ETick = "tick"
eventString ev    = map toLower (show ev)

subscribe' :: FromJSON a => Bool -> FilePath -> [EventT] -> EventHandler a -> IO ()
subscribe' trace socketPath events handler =
  bracket (connect socketPath) close $ \sock -> do
    let req = Request Subscribe eventsJson
        eventsJson = encode $ map eventString events
    (Response _ res) <- invoke' trace sock req
    case res >>= runParser checkSuccess of
      Left err -> throwIO (CommandFailed err)
      Right () -> handleEvents sock
  where handleEvents sock = do
          (Event type' payload) <- recvEvent sock
          case payload of
            Left err -> throwIO (CommandFailed err)
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

connect :: FilePath -> IO Socket
connect sockPath = do
  sock <- Net.socket Net.AF_UNIX Net.Stream 0
  Net.connect sock (Net.SockAddrUnix sockPath)
  pure sock

close :: Socket -> IO ()
close = Net.close

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

recvEvent :: FromJSON a => Socket -> IO (Event a)
recvEvent sock = do
  (type', payload) <- recvPacket sock
  evType <- either pure (\_ -> fail "Expecting Event") type'
  pure (Event evType (eitherDecode payload))
