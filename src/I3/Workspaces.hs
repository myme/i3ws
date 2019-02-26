{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module I3.Workspaces where

import Control.Arrow ((>>>))
import Control.Monad (void, when)
import Data.Aeson (FromJSON, ToJSON, decode)
import Data.ByteString.Lazy.UTF8 (fromString)
import Data.Char
import Data.Foldable
import GHC.Generics
import I3.IPC
import I3.Utils
import Text.ParserCombinators.ReadP
import Text.Read (readMaybe)

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

getWorkspaces :: Invoker inv => inv -> IO [Workspace]
getWorkspaces inv = do
  (Response _ payload) <- invoke inv (Request GetWorkspaces mempty)
  case decode payload of
    Nothing -> fail "Invalid workspace response"
    Just res -> pure res

createWorkspace :: Invoker inv => inv -> String -> IO ()
createWorkspace inv name' =
  let cmd = fromString ("workspace \"" <> name' <> "\"")
  in void $ invoke inv (Request RunCommand cmd)

renumber :: [String] -> [String]
renumber = zipWith newName (map show [1 :: Int ..])
  where newName i old =
          let (_, label) = parseName old
          in if null label then i else i <> ": " <> label

rename :: Invoker inv => inv -> String -> String -> IO ()
rename inv old new = do
  let cmd = "rename workspace \"" <> fromString old <> "\" to \"" <> fromString new <> "\""
  when (old /= new) $ do
    res <- invoke inv (Request RunCommand cmd)
    print res

renameAll :: Invoker inv => inv -> [String] -> [String] -> IO ()
renameAll inv = traverse_ (uncurry $ rename inv) ... zip

moveRight :: Int -> [String] -> [String]
moveRight idx workspaces = case splitAt idx workspaces of
  (_, []) -> workspaces
  (_, [_]) -> workspaces
  (pre, c:n:post) -> pre <> (n : c : post)

moveLeft :: Int -> [String] -> [String]
moveLeft idx workspaces = case splitAt idx workspaces of
  (_, []) -> workspaces
  ([], _) -> workspaces
  (pre, c:post) -> init pre <> (c : last pre : post)

assignWorkspaceNumbers :: Invoker inv => inv -> IO ()
assignWorkspaceNumbers inv = do
  workspaces <- map name <$> getWorkspaces inv
  renameAll inv workspaces (renumber workspaces)

parse :: Show a => ReadP a -> String -> (Maybe a, String)
parse parser input = case readP_to_S parser input of
  [] -> (Nothing, input)
  (res, rest):_ -> (Just res, rest)

parseNumber :: (Num a, Read a) => ReadP (Maybe a)
parseNumber = readMaybe <$> munch1 isDigit

parseName :: String -> (Maybe Int, String)
parseName = parse workspaceName >>> \case
  (Just res, _) -> res
  (Nothing, res) -> (Nothing, res)
  where workspaceName = do
          num' <- option Nothing parseNumber
          label <- parseLabel <++ look
          eof
          pure (num', label)
        colon = char ':'
        parseLabel = (colon +++ satisfy isSpace)
          >> skipSpaces
          >> many get
