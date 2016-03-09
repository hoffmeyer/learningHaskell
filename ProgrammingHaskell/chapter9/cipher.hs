module Cipher where

import Data.Char

caecar :: Int -> String -> String
caecar n = map f
    where f c = case generalCategory c of
            LowercaseLetter -> addChar 'a' n c
            UppercaseLetter -> addChar 'A' n c
            _               -> c

unCaecar n = caecar (-n)

addChar :: Char -> Int -> Char -> Char
addChar b o c = chr $ ord b + (ord c - ord b + o) `mod` 26
