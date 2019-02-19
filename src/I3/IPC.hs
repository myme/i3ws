{-# LANGUAGE OverloadedStrings #-}

module I3.IPC where

import           Control.Monad (when)
import           Data.Aeson (encode)
import           Data.Binary.Get
import           Data.Binary.Put
import           Data.Bits
import           Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy.Char8 as B8
import           Data.Char
import           Network.Socket (Socket)
import qualified Network.Socket as Net
import qualified Network.Socket.ByteString.Lazy as NS
import           System.Process (readProcess)

data RequestT = RunCommand
              | GetWorkspaces
              | ReqSubscribe
              | GetOutputs
              | GetTree
              | GetMarks
              | GetBarConfig
              | GetVersion
              | GetBindingModes
              | GetConfig
              | SendTick
              | Sync
              deriving (Bounded, Enum, Show)

data ResponseT = Command
               | Workspaces
               | ResSubscribe
               | Outputs
               | Tree
               | Marks
               | BarConfig
               | Version
               | BindingModes
               | Config
               | Tick
               deriving (Bounded, Enum, Show)

data EventT = Workspace
            | Output
            | Mode
            | Window
            | BarConfigUpdate
            | Binding
            | Shutdown
            | ETick
            deriving (Bounded, Enum, Show)

data Request = Request RequestT ByteString deriving Show
data Response = Response ResponseT ByteString deriving Show
data Event = Event EventT ByteString deriving Show

debug :: String -> IO ()
debug = when debugEnabled . putStrLn
  where debugEnabled = False

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
  debug ("Sending: " <> show package)
  bytesSent <- NS.send sock package
  debug ("Sent " <> show bytesSent <> " bytes")

decodeHeader :: ByteString -> (Either EventT ResponseT, Int)
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

recv :: Socket -> IO Response
recv sock = do
  let headerLength = B8.length magic + 4 + 4 -- Magic + 2x 32 bit ints
  (type', payloadLength) <- decodeHeader <$> NS.recv sock (fromIntegral headerLength)
  payload <- NS.recv sock (fromIntegral payloadLength)
  resType <- either (\_ -> fail "Unexpected Event") pure type'
  pure (Response resType payload)

recvEvent :: Socket -> IO Event
recvEvent sock = do
  let headerLength = B8.length magic + 4 + 4 -- Magic + 2x 32 bit ints
  (type', payloadLength) <- decodeHeader <$> NS.recv sock (fromIntegral headerLength)
  payload <- NS.recv sock (fromIntegral payloadLength)
  evType <- either pure (\_ -> fail "Expecting Event") type'
  pure (Event evType payload)

subscribe :: [EventT] -> Request
subscribe events = Request ReqSubscribe eventsJson
  where eventsJson = encode $ map (map toLower . show) events

invoke :: Socket -> Request -> IO Response
invoke sock req = do
  send sock req
  recv sock
