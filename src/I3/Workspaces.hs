{-# LANGUAGE DeriveGeneric #-}

module I3.Workspaces
  ( getWorkspaces
  ) where

import Data.Aeson (FromJSON, decode)
import GHC.Generics
import I3.IPC
import I3.Internal

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
