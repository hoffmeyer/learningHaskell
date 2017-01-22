import Prelude hiding (all, any, takeWhile, dropWhile)


all :: (a -> Bool) -> [a] -> Bool
all f = and . map f

any :: (a -> Bool) -> [a] -> Bool
any f = or . map f

takeWhile :: (a -> Bool) -> [a] -> [a]
takeWhile _ [] = []
takeWhile f (x:xs) 
  | f x = x : takeWhile f xs
  | otherwise = []

dropWhile :: (a -> Bool) -> [a] -> [a]
dropWhile _ [] = []
dropWhile f (x:xs) 
  | f x = dropWhile f xs
  | otherwise = x:xs
