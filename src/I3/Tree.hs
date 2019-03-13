{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module I3.Tree where

import Data.Aeson hiding (json)
import Data.Aeson.TH
import Data.Aeson.Types (typeMismatch)
import Data.List (stripPrefix)
import Data.Maybe (fromMaybe, isJust)
import Data.Text (unpack)
import I3.IPC hiding (ResponseT(Tree), Output, Workspace)

data NodeType = Root | Output | Con | FloatingCon | Workspace | Dockarea
              deriving (Eq, Show)

instance FromJSON NodeType where
  parseJSON (String s) = case s of
    "root"         -> pure Root
    "output"       -> pure Output
    "con"          -> pure Con
    "floating_con" -> pure FloatingCon
    "workspace"    -> pure Workspace
    "dockarea"     -> pure Dockarea
    x              -> fail ("Invalid node type" <> unpack x)
  parseJSON x = typeMismatch "NodeTYpe" x

data WindowProps = WindowProps
  { win_class :: String
  , win_instance :: String
  , win_title :: String
  } deriving (Eq, Show)

$(deriveFromJSON
 defaultOptions {
     fieldLabelModifier = \n -> fromMaybe n (stripPrefix "win_" n) }
 ''WindowProps)

data Node = Node { node_id :: Int
                 , node_name :: Maybe String
                 , node_type :: NodeType
                 , node_nodes :: [Node]
                 , node_floating_nodes :: [Node]
                 , node_window :: Maybe Int
                 , node_window_properties :: Maybe WindowProps
                 }
  deriving (Eq, Show)

$(deriveFromJSON
  defaultOptions {
     fieldLabelModifier = \n -> fromMaybe n (stripPrefix "node_" n) }
 ''Node)

getTree :: Invoker inv => inv -> IO Node
getTree inv = do
  (Response _ json) <- invoke inv (Request GetTree mempty)
  case eitherDecode json of
    Left err -> fail err
    Right tree -> pure tree

flatten :: Node -> [Node]
flatten root =
  root :
  concatMap flatten (node_nodes root) <>
  concatMap flatten (node_floating_nodes root)

leaves :: Node -> [Node]
leaves = filter isLeaf . flatten
  where isLeaf = isJust . node_window

-- TODO: Use getWorkspaces from Workspaces to determine active/visible
-- workspaces, ignoring the dock area, etc.
workspaces :: Node -> [Node]
workspaces = filter isWorkspace . flatten
  where isWorkspace = (== Workspace) . node_type
