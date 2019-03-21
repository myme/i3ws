module I3WS.Workspaces where

import Control.Arrow ((>>>))
import Data.Char (isDigit, isSpace)
import Data.Semigroup hiding (option)
import I3.IPC
import I3.Workspaces
import Text.ParserCombinators.ReadP
import Text.Read (readMaybe)

renumber :: [String] -> [String]
renumber = zipWith newName' (map show [1 :: Int ..])
  where newName' i old =
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
moveRight :: Invoker -> IO ()
moveRight inv = do
  wss <- either fail id <$> getWorkspaces inv
  res <- renameAll inv (foldMap reorder' $ zip wss (drop 1 wss))
  either fail pure res
  where reorder' (l, r) | focused l = swap l r
                        | otherwise = []

-- | Move current workspace one position to the left.
moveLeft :: Invoker -> IO ()
moveLeft inv = do
  wss <- either fail id <$> getWorkspaces inv
  res <- renameAll inv (foldMap reorder' $ zip wss (drop 1 wss))
  either fail pure res
  where reorder' (l, r) | focused r = swap l r
                        | otherwise = []

moveNew :: Invoker -> IO ()
moveNew inv = newName inv >>= moveContainer inv >>= either fail pure

newName :: Invoker -> IO String
newName inv = do
  let mklast = fmap Last . fst . parseName . name
  maybe "1" (show . (+1) . getLast) . foldMap mklast . either fail id <$> getWorkspaces inv

newWorkspace inv = newName inv >>= createWorkspace inv >>= either fail pure
newWorkspace :: Invoker -> IO ()

-- | Assigns sequential numbers to all workspaces.
assignNumbers :: Invoker -> IO ()
assignNumbers inv = do
  wss <- either fail (map name) <$> getWorkspaces inv
  res <- renameAll inv (zip wss (renumber wss))
  either fail pure res

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
