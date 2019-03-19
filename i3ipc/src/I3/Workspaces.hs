{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module I3.Workspaces where

import Control.Monad (void, when)
import Data.Aeson (FromJSON, ToJSON, decode)
import Data.ByteString.Lazy.UTF8 (fromString)
import Data.Foldable
import GHC.Generics
import I3.IPC

data Geometry = Geometry
  { x :: Int
  , y :: Int
  , width :: Int
  , height :: Int
  } deriving (Eq, Generic, Show)

instance FromJSON Geometry
instance ToJSON Geometry

data Workspace = Workspace
  { num :: Int
  , name :: String
  , focused :: Bool
  , visible :: Bool
  , rect :: Geometry
  , output :: String
  , urgent :: Bool
  } deriving (Eq, Generic, Show)

instance FromJSON Workspace
instance ToJSON Workspace

getWorkspaces :: Invoker -> IO [Workspace]
getWorkspaces inv = do
  (Response _ payload) <- invoke inv (Request GetWorkspaces mempty)
  case decode payload of
    Nothing -> fail "Invalid workspace response"
    Just res -> pure res

createWorkspace :: Invoker -> String -> IO ()
createWorkspace inv name' = do
  let cmd = fromString ("workspace \"" <> name' <> "\"")
  void $ invoke inv (Request RunCommand cmd)

moveContainer :: Invoker -> String -> IO ()
moveContainer inv name' = do
  let cmd = fromString ("move container to workspace \"" <> name' <> "\"")
  void $ invoke inv (Request RunCommand cmd)

rename :: Invoker -> String -> String -> IO ()
rename inv old new = do
  let cmd = "rename workspace \"" <> fromString old <> "\" to \"" <> fromString new <> "\""
  when (old /= new) (void $ invoke inv (Request RunCommand cmd))

renameAll :: Invoker -> [(String, String)] -> IO ()
renameAll inv = traverse_ (uncurry $ rename inv)
