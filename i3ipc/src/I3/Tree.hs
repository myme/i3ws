{-# LANGUAGE TemplateHaskell #-}

module I3.Tree where

import Control.Monad.Catch
import Control.Monad.IO.Class
import Data.Aeson hiding (json)
import Data.Aeson.TH
import Data.Aeson.Types (typeMismatch)
import Data.List (isPrefixOf, stripPrefix)
import Data.Maybe (fromMaybe, isJust)
import Data.Text (unpack)
import I3.IPC hiding (Output, Workspace)

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

instance ToJSON NodeType where
  toJSON Root        = "root"
  toJSON Output      = "output"
  toJSON Con         = "con"
  toJSON FloatingCon = "floating_con"
  toJSON Workspace   = "workspace"
  toJSON Dockarea    = "dockarea"

data WindowProps = WindowProps
  { win_class :: Maybe String
  , win_instance :: Maybe String
  , win_title :: Maybe String
  } deriving (Eq, Show)

$(deriveJSON
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

$(deriveJSON
  defaultOptions {
     fieldLabelModifier = \n -> fromMaybe n (stripPrefix "node_" n) }
 ''Node)

getTree :: (MonadIO m, MonadThrow m) => Invoker m -> m Node
getTree inv = invoke inv (Request Tree mempty)

flatten :: Node -> [Node]
flatten root =
  root :
  concatMap flatten (node_nodes root) <>
  concatMap flatten (node_floating_nodes root)

leaves :: Node -> [Node]
leaves = filter isLeaf . flatten
  where isLeaf = isJust . node_window

workspaces :: Node -> [Node]
workspaces = filter isWorkspace . flatten
  where isWorkspace ws = node_type ws == Workspace && not (isPrivate ws)
        isPrivate = maybe False (isPrefixOf "__") . node_name
