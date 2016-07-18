module AsPattern where

import Data.Char


isSubsequenceOf :: (Eq a) => [a] -> [a] -> Bool
isSubsequenceOf [] _ = True
isSubsequenceOf _ [] = False
isSubsequenceOf xs'@(x:xs) (y:ys) = if x == y 
                                           then isSubsequenceOf xs ys 
                                           else isSubsequenceOf xs' ys

test1 = isSubsequenceOf "blah" "blahwoot"
test2 = isSubsequenceOf "blah" "wootblah"
test3 = isSubsequenceOf "blah" "wboloath"
test4 = isSubsequenceOf "blah" "wootbla"

capitalizeWords :: String -> [(String, String)]
capitalizeWords s = map f (words s) where
        f xs'@(x:xs) = (xs', toUpper x : xs)
