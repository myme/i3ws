module I3WS.Workspaces where

import Control.Arrow ((>>>))
import Control.Monad.Catch (bracket_)
import Data.Aeson
import Data.Char (isDigit, isSpace)
import Data.Semigroup hiding (option)
import I3.IPC
import I3.Workspaces
import I3WS.Types
import Text.ParserCombinators.ReadP
import Text.Read (readMaybe)

-- | Bracket an IO action with tick events to temporarily disable event listening.
withEventsIgnored :: I3WS a -> I3WS a
withEventsIgnored = bracket_ (setIgnore True) (setIgnore False)
  where payload   i = encode $ object ["ignoreEvents" .= i]
        setIgnore i = do
          inv <- i3ws_invoker <$> ask
          runParser checkSuccess <$> invoke inv (Request Tick (payload i))

renumber :: String -> [String] -> [String]
renumber separator = zipWith newName' (map show [1 :: Int ..])
  where newName' i old =
          let (_, label) = parseName old
          in if null label then i else i <> separator <> label

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
moveRight :: I3WS ()
moveRight = withEventsIgnored $ do
  inv <- i3ws_invoker <$> ask
  wss <- liftIO (getWorkspaces inv)
  renameAll inv (foldMap reorder' $ zip wss (drop 1 wss))
  where reorder' (l, r) | focused l = swap l r
                        | otherwise = []

-- | Move current workspace one position to the left.
moveLeft :: I3WS ()
moveLeft = withEventsIgnored $ do
  inv <- i3ws_invoker <$> ask
  wss <- getWorkspaces inv
  renameAll inv (foldMap reorder' $ zip wss (drop 1 wss))
  where reorder' (l, r) | focused r = swap l r
                        | otherwise = []

moveNew :: I3WS ()
moveNew = do
  inv <- i3ws_invoker <$> ask
  newName >>= moveContainer inv

newName :: I3WS String
newName = do
  inv <- i3ws_invoker <$> ask
  let mklast = fmap Last . fst . parseName . name
  maybe "1" (show . (+1) . getLast) . foldMap mklast <$> getWorkspaces inv

newWorkspace :: I3WS ()
newWorkspace = do
  inv <- i3ws_invoker <$> ask
  newName >>= createWorkspace inv

-- | Assigns sequential numbers to all workspaces.
assignNumbers :: I3WS ()
assignNumbers = do
  inv <- i3ws_invoker <$> ask
  wss <- map name <$> getWorkspaces inv
  separator <- i3ws_separator <$> ask
  renameAll inv (zip wss (renumber separator wss))

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
