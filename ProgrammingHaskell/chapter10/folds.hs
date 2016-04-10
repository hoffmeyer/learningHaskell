module Folds where

myAnd :: [Bool] -> Bool
myAnd = foldr (&&) True

myOr :: [Bool] -> Bool
myOr = foldr (||) False

myAny :: (a -> Bool) -> [a] -> Bool
myAny f = foldr ((||).f) False

myElemRec :: Eq a => a -> [a] -> Bool
myElemRec _ [] = False
myElemRec n (x:xs) = (n == x) || myElemRec n xs 

myElemFold :: Eq a => a -> [a] -> Bool
myElemFold n = foldr (\ a b -> b || a == n) False

