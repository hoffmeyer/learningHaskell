import Data.Char

filterUpper = filter isUpper

capitalize [] = []
capitalize (x:xs) = toUpper x : xs

allCaps = map toUpper

allCaps' [] = []
allCaps' (x:xs) = toUpper x : allCaps xs

firstAsCap = toUpper . head
