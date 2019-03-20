#!/usr/bin/env stack
{-
stack script
  --resolver lts-13.12
  --package aeson
  --package containers
  --package process
  --package text
-}

import           Control.Arrow
import           Data.Char
import qualified Data.Map.Strict as M
import           Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.IO as Tio
import           System.Process

type FaMap = M.Map T.Text T.Text

iconName :: T.Text -> T.Text
iconName = T.splitOn "-" >>> map T.toTitle >>> T.concat >>> addPrefix
  where addPrefix x | isAlpha (T.head x) = x
                    | otherwise = "FA_" <> x

parseSCSS :: T.Text -> FaMap
parseSCSS =
  T.lines >>>
  mapMaybe (T.stripPrefix "$fa-var-") >>>
  map (T.splitOn ": " >>> \[k, v] -> (iconName k, fromCSSCodepoint v)) >>>
  M.fromList
  where
    fromCSSCodepoint = T.dropWhileEnd (/= '"') >>>
                       T.replace "\\" "\\x" >>>
                       T.unpack >>>
                       read

scssURL :: String
scssURL = "https://raw.githubusercontent.com/FortAwesome/Font-Awesome/fa-4/scss/_variables.scss"

getSCSS :: String -> IO FaMap
getSCSS url = parseSCSS . T.pack <$> readProcess "curl" ["-L", url] ""

generate :: FaMap -> T.Text
generate icons = T.intercalate "\n" $
  [ "module FontAwesome.Icons where"
  , ""
  , "data FontAwesome"
  , "  = " <> T.intercalate "\n  | " (M.keys icons)
  , ""
  , ""
  , "icon :: FontAwesome -> String"
  , "icon fa = case fa of"
  ] <>
  map (\(k, v) -> "  " <> k <> " -> " <> T.pack (show v)) (M.toAscList icons)

main :: IO ()
main = getSCSS scssURL >>= Tio.putStrLn . generate
