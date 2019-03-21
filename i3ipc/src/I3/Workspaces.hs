module I3.Workspaces where

import Data.Aeson (FromJSON, ToJSON)
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

getWorkspaces :: Invoker -> IO (Either String [Workspace])
getWorkspaces inv = invoke inv (Request Workspaces mempty)

createWorkspace :: Invoker -> String -> IO (Either String ())
createWorkspace inv name' = do
  let cmd = fromString ("workspace \"" <> name' <> "\"")
  invoke inv (Request Command cmd)

moveContainer :: Invoker -> String -> IO (Either String ())
moveContainer inv name' = do
  let cmd = fromString ("move container to workspace \"" <> name' <> "\"")
  invoke inv (Request Command cmd)

rename :: Invoker -> String -> String -> IO (Either String ())
rename inv old new = do
  let cmd = "rename workspace \"" <> fromString old <> "\" to \"" <> fromString new <> "\""
  if old == new
    then pure (Right ())
    else invoke inv (Request Command cmd)

renameAll :: Invoker -> [(String, String)] -> IO ()
renameAll inv = traverse_ (uncurry $ rename inv)
