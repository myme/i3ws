{-# LANGUAGE LambdaCase #-}

module I3WS.Workspaces where

import Control.Arrow ((>>>))
import Data.Char (isDigit, isSpace)
import Data.Semigroup hiding (option)
import I3.IPC
import I3.Workspaces
import Text.ParserCombinators.ReadP
import Text.Read (readMaybe)

renumber :: [String] -> [String]
renumber = zipWith newName (map show [1 :: Int ..])
  where newName i old =
          let (_, label) = parseName old
          in if null label then i else i <> ":" <> label

-- | Generate rename commands for swapping two workspaces.
swap :: Workspace -> Workspace -> [(String, String)]
swap l r = [(name r, tmp)
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
  where reorder' (l, r) | focused l = swap l r
                        | otherwise = []

-- | Move current workspace one position to the left.
moveLeft :: Invoker inv => inv -> IO ()
moveLeft inv = do
  ws <- getWorkspaces inv
  renameAll inv (foldMap reorder' $ zip ws (drop 1 ws))
  where reorder' (l, r) | focused r = swap l r
                        | otherwise = []

newWorkspace :: Invoker inv => inv -> IO ()
newWorkspace inv = do
  let mklast = fmap Last . fst . parseName . name
  newNum <- maybe 1 ((+1) . getLast) . foldMap mklast <$> getWorkspaces inv
  createWorkspace inv (show newNum)

-- | Assigns sequential numbers to all workspaces.
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
          num'  <- option Nothing parseNumber
          label <- parseLabel <++ look
          eof
          pure (num', label)
        colon = char ':'
        parseLabel = (colon +++ satisfy isSpace)
          >> skipSpaces
          >> many get
