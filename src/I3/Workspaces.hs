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
createWorkspace inv name' = do
  let cmd = fromString ("workspace \"" <> name' <> "\"")
  void $ invoke inv (Request RunCommand cmd)
  assignNumbers inv

renumber :: [String] -> [String]
renumber = zipWith newName (map show [1 :: Int ..])
  where newName i old =
          let (_, label) = parseName old
          in if null label then i else i <> ":" <> label

rename :: Invoker inv => inv -> String -> String -> IO ()
rename inv old new = do
  let cmd = "rename workspace \"" <> fromString old <> "\" to \"" <> fromString new <> "\""
  when (old /= new) (void $ invoke inv (Request RunCommand cmd))

renameAll :: Invoker inv => inv -> [(String, String)] -> IO ()
renameAll inv = traverse_ (uncurry $ rename inv)

-- | Generate rename commands for swapping two workspaces.
reorder :: Workspace -> Workspace -> [(String, String)]
reorder l r = [(name r, tmp)
              ,(name l, concatName rn lm)
              ,(tmp, concatName ln rm)
              ]
  where (ln, lm) = parseName (name l)
        (rn, rm) = parseName (name r)
        tmp = concatName rn "tmp"
        concatName n m = maybe "" show n <> (':':m)

-- | Move current workspace one position to the right.
moveRight :: Invoker inv => inv -> IO ()
moveRight inv = do
  ws <- getWorkspaces inv
  renameAll inv (foldMap reorder' $ zip ws (drop 1 ws))
  where reorder' (l, r) | focused l = reorder l r
                        | otherwise = []

-- | Move current workspace one position to the left.
moveLeft :: Invoker inv => inv -> IO ()
moveLeft inv = do
  ws <- getWorkspaces inv
  renameAll inv (foldMap reorder' $ zip ws (drop 1 ws))
  where reorder' (l, r) | focused r = reorder l r
                        | otherwise = []

assignNumbers :: Invoker inv => inv -> IO ()
assignNumbers inv = do
  workspaces <- map name <$> getWorkspaces inv
  renameAll inv (zip workspaces (renumber workspaces))

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
