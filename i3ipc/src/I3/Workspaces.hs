module I3.Workspaces where

import Control.Monad (when)
import Data.Aeson (FromJSON, ToJSON)
import Data.Foldable (traverse_)
import GHC.Generics
import I3.Command
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
getWorkspaces inv = invoke inv (Request Workspaces mempty)

createWorkspace :: Invoker -> String -> IO ()
createWorkspace inv name' = command inv ("workspace \"" <> name' <> "\"")

moveContainer :: Invoker -> String -> IO ()
moveContainer inv name' = do
  let cmd = "move container to workspace \"" <> name' <> "\""
  command inv cmd

rename :: Invoker -> String -> String -> IO ()
rename inv old new = do
  let cmd = "rename workspace \"" <> old <> "\" to \"" <> new <> "\""
  when (old /= new) $ command inv cmd

renameAll :: Invoker -> [(String, String)] -> IO ()
renameAll inv = traverse_ (uncurry $ rename inv)
