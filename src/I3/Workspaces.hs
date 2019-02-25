{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module I3.Workspaces where

import Control.Arrow ((>>>))
import Control.Monad (when)
import Data.Aeson (FromJSON, decode)
import Data.ByteString.Lazy.UTF8 (fromString)
import Data.Char
import Data.Foldable
import GHC.Generics
import I3.IPC
import I3.Internal
import Text.ParserCombinators.ReadP
import Text.Read (readMaybe)

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

renumber :: [String] -> [String]
renumber = zipWith newName (map show [1 :: Int ..])
  where newName i old =
          let (_, label) = parseName old
          in if null label then i else i <> ": " <> label

rename :: I3 -> String -> String -> IO ()
rename i3 old new = do
  let sock = i3CmdSocket i3
      cmd = "rename workspace \"" <> fromString old <> "\" to \"" <> fromString new <> "\""
  when (old /= new) $ do
    res <- invoke sock (Request RunCommand cmd)
    print res

moveRight :: Int -> [String] -> [String]
moveRight idx workspaces = renumber $
  case splitAt idx workspaces of
    (_, []) -> workspaces
    (_, [_]) -> workspaces
    (pre, c:n:post) -> pre <> (n : c : post)

moveLeft :: Int -> [String] -> [String]
moveLeft idx workspaces = renumber $
  case splitAt idx workspaces of
    (_, []) -> workspaces
    ([], _) -> workspaces
    (pre, c:post) -> init pre <> (c : last pre : post)

assignWorkspaceNumbers :: I3 -> IO ()
assignWorkspaceNumbers i3 = do
  workspaces <- map name <$> getWorkspaces i3
  let renames = zip workspaces (renumber workspaces)
  traverse_ (uncurry $ rename i3) renames

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
