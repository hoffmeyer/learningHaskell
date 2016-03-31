module Folds where

myAnd :: [Bool] -> Bool
myAnd = foldr (&&) True
