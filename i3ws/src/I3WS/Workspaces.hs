module I3WS.Workspaces where

import Control.Arrow ((>>>))
import Control.Exception (bracket_)
import Data.Aeson
import Data.Char (isDigit, isSpace)
import Data.Semigroup hiding (option)
import I3.IPC
import I3.Workspaces
import Text.ParserCombinators.ReadP
import Text.Read (readMaybe)

-- | Bracket an IO action with tick events to temporarily disable event listening.
withEventsIgnored :: Invoker -> IO a -> IO a
withEventsIgnored inv = bracket_ (setIgnore True) (setIgnore False)
  where payload   i = encode $ object ["ignoreEvents" .= i]
        setIgnore i = runParser checkSuccess <$> invoke inv (Request Tick (payload i))

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
moveRight inv = withEventsIgnored inv $ do
  wss <- getWorkspaces inv
  renameAll inv (foldMap reorder' $ zip wss (drop 1 wss))
  where reorder' (l, r) | focused l = swap l r
                        | otherwise = []

-- | Move current workspace one position to the left.
moveLeft :: Invoker -> IO ()
moveLeft inv = withEventsIgnored inv $ do
  wss <- getWorkspaces inv
  renameAll inv (foldMap reorder' $ zip wss (drop 1 wss))
  where reorder' (l, r) | focused r = swap l r
                        | otherwise = []

moveNew :: Invoker -> IO ()
moveNew inv = newName inv >>= moveContainer inv

newName :: Invoker -> IO String
newName inv = do
  let mklast = fmap Last . fst . parseName . name
  maybe "1" (show . (+1) . getLast) . foldMap mklast <$> getWorkspaces inv

newWorkspace :: Invoker -> IO ()
newWorkspace inv = newName inv >>= createWorkspace inv

-- | Assigns sequential numbers to all workspaces.
assignNumbers :: Invoker -> IO ()
assignNumbers inv = do
  wss <- map name <$> getWorkspaces inv
  renameAll inv (zip wss (renumber wss))

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
