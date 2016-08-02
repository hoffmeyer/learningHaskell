module Section12_5 where

import Data.Char
import Data.Maybe

-- STring processing
notThe :: String -> Maybe String
notThe s = case map toLower s of
             "the" -> Nothing
             _ -> Just s

replaceThe :: String -> String
replaceThe s = unwords . map( Section12_5.fromMaybe "a" . notThe) $ words s

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

-- Validate the word
newtype Word' = Word' String deriving (Eq, Show)

mkWord :: String -> Maybe Word'
mkWord s = if vowels > (length s - vowels)
              then Nothing
              else Just $ Word' s
                      where
                              vowels = countVowels s
-- It's only Natural
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

-- Small library for Maybe
isJust :: Maybe a -> Bool
isJust Nothing = False
isJust (Just _) = True

isNothing :: Maybe a -> Bool
isNothing = not . Section12_5.isJust

mayybee :: b -> (a -> b) -> Maybe a -> b
mayybee z f Nothing = z
mayybee z f (Just x) = f x

fromMaybe :: a -> Maybe a -> a
fromMaybe x Nothing = x
fromMaybe x (Just y) = y

listToMaybe :: [a] -> Maybe a
listToMaybe [] = Nothing
listToMaybe (a:as) = Just a

maybeToList :: Maybe a -> [a]
maybeToList Nothing = []
maybeToList (Just x) = [x]

catMaybes :: [Maybe a] -> [a]
catMaybes [] = []
catMaybes (Nothing:ms) = Section12_5.catMaybes ms
catMaybes (Just x:ms) = x : Section12_5.catMaybes ms

flipMaybe :: [Maybe a] -> Maybe [a]
flipMaybe [] = Just []
flipMaybe ms= case flip ms of
                [] -> Nothing
                x -> Just x
              where
                      flip [] = []
                      flip (Nothing:ms) = []
                      flip (Just x:ms) = x:flip ms

-- Small library for Either

lefts' :: [Either a b] -> [a]
lefts' = foldr toLeft []
        where
                toLeft (Left x) xs = x:xs
                toLeft (Right _) xs = xs

rights' :: [Either a b] -> [b]
rights' = foldr toRight []
        where
                toRight (Right x) xs = x:xs
                toRight (Left _) xs = xs

partitionEithers' :: [Either a b] -> ([a], [b])
partitionEithers' = foldr partition' ([],[])
        where
                partition' (Left a) (as, bs) = (a:as, bs)
                partition' (Right b) (as, bs) = (as, b:bs)

eitherMaybe' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe' f (Left _) = Nothing
eitherMaybe' f (Right b) = Just $ f b

either' :: (a -> c) -> (b -> c) -> Either a b -> c
either' f _ (Left a) = f a
either' _ g (Right b) = g b

eitherMaybe'' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe'' f = either' (const Nothing) (Just . f)

-- Iterate and unfoldr
myIterate :: (a -> a) -> a -> [a]
myIterate f x = x : myIterate f (f x)

myUnfoldr :: (b -> Maybe (a, b)) -> b -> [a]
myUnfoldr f x = case f x of
                  Nothing -> []
                  Just (i, j) -> i : myUnfoldr f j

betterIterate :: (a -> a) -> a -> [a]
betterIterate f = myUnfoldr (\ x -> Just (x, f x))
