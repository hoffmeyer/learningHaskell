module Vigenere where

import Data.Char

keyword = concat $ repeat "flemming"

vigenere :: String -> String -> String
vigenere keyword text = map f zipped
        where zipped = zip (concat $ repeat keyword) text
              f (k,t) = case generalCategory t of
                          LowercaseLetter -> addChar 'a' (getOffset k) t
                          UppercaseLetter -> addChar 'A' (getOffset k) t
                          _               -> t

addChar :: Char -> Int -> Char -> Char
addChar b o c = chr $ ord b + (ord c - ord b + o) `mod` 26

getOffset :: Char -> Int
getOffset c = case generalCategory c of
                  LowercaseLetter -> ord c - ord 'a'
                  UppercaseLetter -> ord c - ord 'A'
                  _               -> 0
