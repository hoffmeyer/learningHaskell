module Section12_5 where

import Data.Char
import Data.Maybe

notThe :: String -> Maybe String
notThe s = case map toLower s of
             "the" -> Nothing
             _ -> Just s

replaceThe :: String -> String
replaceThe s = unwords . map( fromMaybe "a" . notThe) $ words s

isVowel :: Char -> Bool
isVowel c = c `elem` "aeiou"


countTheBeforeVowel :: String -> Integer
countTheBeforeVowel s = countThe . words $ s
        where
                countThe [word] = 0
                countThe (fst:rest@(snd:_)) = if fst == "the" && isVowel (head snd)
                                                 then 1 + countThe rest
                                                 else countThe rest

vowelsInString :: String -> String
vowelsInString = filter isVowel

countVowels :: String -> Int
countVowels = length . vowelsInString

newtype Word' = Word' String deriving (Eq, Show)

mkWord :: String -> Maybe Word'
mkWord s = if vowels > (length s - vowels)
              then Nothing
              else Just $ Word' s
                      where
                              vowels = countVowels s

data Nat = Zero
         | Succ Nat
         deriving (Eq, Show)

natToInteger :: Nat -> Integer
natToInteger Zero = 0
natToInteger (Succ n) = 1 + natToInteger n

integerToNat :: Integer -> Maybe Nat
integerToNat n
  | n < 0 = Nothing
  | n >= 0 = Just (toNat n)
        where
                toNat 0 = Zero
                toNat x = Succ (toNat (x-1))

