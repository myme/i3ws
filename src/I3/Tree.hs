{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module I3.Tree where

import Data.Aeson (eitherDecode, FromJSON(..), ToJSON(..), Value(..))
import Data.Aeson.TH (deriveJSON, defaultOptions, Options(fieldLabelModifier))
import Data.Aeson.Types (typeMismatch)
import Data.Text (unpack)
import GHC.Generics (Generic)
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

instance ToJSON NodeType where
  toJSON _ = undefined

data Tree = Tree { id :: Int
                 , name :: Maybe String
                 , node_type :: NodeType
                 , nodes :: [Tree]
                 }
  deriving (Eq, Generic, Show)

$(deriveJSON defaultOptions
  { fieldLabelModifier = \n -> if n == "node_type" then "type" else n }
  ''Tree)

getTree :: Invoker inv => inv -> IO Tree
getTree inv = do
  (Response _ json) <- invoke inv (Request GetTree mempty)
  case eitherDecode json of
    Left err -> fail err
    Right tree -> pure tree
