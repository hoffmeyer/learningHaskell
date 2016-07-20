module LangaugeEx where

import Data.Char
import Data.List
import Data.List.Split

capitalizeWord :: String -> String
capitalizeWord (x:xs) = toUpper x : xs

capitalizeWords :: String -> String
capitalizeWords s = unwords $ map capitalizeWord (words s)

capitalizeParagraph :: String -> String
capitalizeParagraph s = intercalate ". "  (map capitalizeWord (splitOn ". " s))
