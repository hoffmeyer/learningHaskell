module Recursion where

sumUp :: (Eq a, Num a) => a -> a
sumUp 0 = 0
sumUp x = x + sumUp (x - 1)

recMult :: (Integral a) => a -> a -> a
recMult x 1 = x
recMult x y = x + recMult x (y - 1)

mc91 :: (Num a, Ord a) => a -> a
mc91 n
    | n > 100 = n - 10
    | otherwise = mc91 $ mc91 (n + 11)
