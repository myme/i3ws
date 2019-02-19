{-# LANGUAGE DeriveGeneric #-}

module I3
  ( initI3
  , command
  , getWorkspaces
  , subscribeEvents
  , EventT(..)
  , Event(..)
  ) where

import           Control.Exception (bracket)
import           Control.Monad (unless, forever, void)
import           Data.Aeson (FromJSON, decode)
import           Data.ByteString.Lazy.Char8 (pack)
import qualified Data.Map.Strict as Map
import           Data.Maybe
import           GHC.Generics
import           I3.IPC
import           Network.Socket (Socket)

type EventHandler = Event -> IO ()

data I3 = I3
  { i3SocketPath :: FilePath
  , i3CmdSocket :: Socket
  }

initI3 :: IO I3
initI3 = do
  socketPath <- getSocketPath
  cmdSock <- connect socketPath
  pure $ I3 { i3SocketPath = socketPath
            , i3CmdSocket = cmdSock
            }

command :: I3 -> String -> IO ()
command i3 cmd = do
  let sock = i3CmdSocket i3
  void $ invoke sock (Request RunCommand (pack cmd))

data Geometry = Geometry
  { x :: Int
  , y :: Int
  , width :: Int
  , height :: Int
  } deriving (Generic, Show)

instance FromJSON Geometry

data Workspace = Workspace
  { num :: Int
  , name :: String
  , focused :: Bool
  , visible :: Bool
  , rect :: Geometry
  , output :: String
  , urgent :: Bool
  } deriving (Generic, Show)

instance FromJSON Workspace

getWorkspaces :: I3 -> IO [Workspace]
getWorkspaces i3 = do
  let sock = i3CmdSocket i3
  (Response _ payload) <- invoke sock (Request GetWorkspaces mempty)
  case decode payload of
    Nothing -> fail "Invalid workspace response"
    Just res -> pure res

subscribeEvents :: I3 -> [EventT] -> EventHandler -> IO ()
subscribeEvents i3 events handler = do
  let socketPath = i3SocketPath i3
  bracket (connect socketPath) close $ \sock -> do
    (Response _ payload) <- invoke sock (subscribe events)
    let success = fromMaybe False (decode payload >>= Map.lookup "success")
    unless success $ fail "Event subscription failed!"
    forever (recvEvent sock >>= handler)
