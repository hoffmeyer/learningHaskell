module Recursion where

sumUp :: (Eq a, Num a) => a -> a
sumUp 0 = 0
sumUp x = x + sumUp (x - 1)

recMult :: (Integral a) => a -> a -> a
recMult x 1 = x
recMult x y = x + recMult x (y - 1)
