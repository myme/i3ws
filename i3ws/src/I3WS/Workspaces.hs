module I3WS.Workspaces where

import Control.Arrow ((>>>))
import Control.Monad.Catch (bracket_)
import Data.Aeson
import Data.Char (isDigit, isSpace)
import Data.List (find, (\\))
import Data.Maybe (mapMaybe)
import I3.IPC
import I3.Workspaces
import I3WS.Types
import I3WS.Utils (headMaybe, lastMaybe)
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
  where
    newName' i old =
      let (_, label) = parseName separator old
       in if null label then i else i <> separator <> label

-- | Change the label of a workspace.
changeLabel :: String -> String -> String -> String
changeLabel separator oldName newLabel =
  let (index, _) = parseName separator oldName
   in case index of
        Just i -> show i <> separator <> newLabel
        Nothing -> newLabel

-- | Generate rename commands for swapping two workspaces.
swap :: String -> Workspace -> Workspace -> [(String, String)]
swap separator l r =
  [(name r, tmp)
  ,(name l, concatName rn lm)
  ,(tmp, concatName ln rm)
  ]
  where (ln, lm) = parseName separator (name l)
        (rn, rm) = parseName separator (name r)
        tmp = concatName rn "tmp"
        concatName n m = maybe "" show n <> (separator <> m)

-- | Move the focused workspace to a new index.
move :: (Int -> Int) -> I3WS ()
move updateIndex = withEventsIgnored $ do
  inv <- i3ws_invoker <$> ask
  wss <- getWorkspaces inv
  case find focused wss of
    Nothing -> pure ()
    Just focusedWs -> do
      separator <- i3ws_separator <$> ask
      let
        (index, label) = parseName separator (name focusedWs)
        nextIndex = updateIndex <$> index
        otherWs = find ((==) nextIndex . fst . parseName separator . name) wss
      case nextIndex of
        Nothing -> pure ()
        Just nextIndex' ->
          case otherWs of
            Just other -> renameAll inv (swap separator other focusedWs)
            Nothing -> renameAll inv [(name focusedWs, show nextIndex' <> separator <> label)]


-- | Move current workspace one position to the right.
moveRight :: I3WS ()
moveRight = move nextIndex
  where
    nextIndex 10 = 1
    nextIndex i = i + 1

-- | Move current workspace one position to the left.
moveLeft :: I3WS ()
moveLeft = move nextIndex
  where
    nextIndex 1 = 10
    nextIndex i = i - 1

moveNew :: I3WS ()
moveNew = do
  inv <- i3ws_invoker <$> ask
  newName >>= moveContainer inv

newName :: I3WS String
newName = do
  inv <- i3ws_invoker <$> ask
  separator <- i3ws_separator <$> ask
  indices <- mapMaybe (fst . parseName separator . name) <$> getWorkspaces inv
  case lastMaybe indices of
    Nothing -> pure "1"
    Just lastIndex ->
      case headMaybe $ [1 .. lastIndex] \\ indices of
        Nothing -> pure $ show $ lastIndex + 1
        Just i -> pure (show i)

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

parseName :: String -> String -> (Maybe Int, String)
parseName separator = parse workspaceName >>> \case
  (Just res, _) -> res
  (Nothing, res) -> (Nothing, res)
  where workspaceName = do
          num'  <- option Nothing parseNumber
          label <- parseLabel <++ look
          eof
          pure (num', label)
        parseLabel = (string separator +++ munch1 isSpace)
          >> skipSpaces
          >> many get
