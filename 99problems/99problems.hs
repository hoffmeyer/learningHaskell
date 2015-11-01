import Data.List
-- 1
myLast :: [a] -> a
myLast [] = error "No end for empty lists"
myLast [x] = x
myLast (_:xs) = myLast xs

-- 2
myButLast :: [a] -> a
myButLast [] = error "No second last element for empty lists"
myButLast [_] = error "No second last element for lists of length 1"
myButLast [x,y] = x
myButLast (_:xs) = myButLast xs

-- 3
elementAt :: [a] -> Int -> a
elementAt xs 0 = error "Index should be larger than 0"
elementAt xs i = xs !! (i - 1)

-- 4
myLength :: [a] -> Int
myLength [] = 0
myLength (_:xs) = 1 + myLength xs

-- 5
myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = myReverse xs ++ [x]
myReverse' = foldl (flip (:)) []

-- 6
isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome [] = True
isPalindrome [_] = True
isPalindrome (x:xs) = if x /= last xs
                        then False
                        else isPalindrome (init xs)
isPalindrome' xs = xs == (reverse xs)

-- 7
data NestedList a = Elem a | List [NestedList a]
flatten :: NestedList a -> [a]
flatten (Elem x) = [x]
flatten (List []) = []
flatten (List [x]) = flatten x
flatten (List (x:xs)) = flatten x ++ flatten (List xs)

flatten' (Elem x) = [x]
flatten' (List x) = concatMap flatten x

-- 8
compress :: Eq a => [a] -> [a]
compress [] = []
compress (x:xs) = x : (compress $ dropWhile( == x) xs)

-- 9
pack x = group x
pack' [] = []

-- 10
encode :: Eq a => [a] -> [(Int, a)]
encode [] = []
encode xs = map compressElem ( pack xs )
    where compressElem xs = ( length xs, head xs)
encode' xs = [(length x, head x) | x <- group xs]

-- 11
data Elem a = Multiple Int a | Single a 
    deriving (Show)
encodeModified :: Eq a => [a] -> [Elem a]
encodeModified = map encodeHelper . encode
    where
        encodeHelper (1,x) = Single x
        encodeHelper (n,x) = Multiple n x

-- 12
decodeModified :: [Elem a] -> [a]
decodeModified = concatMap decodeHelper 
    where
        decodeHelper :: Elem a -> [a]
        decodeHelper (Single x) = [x]
        decodeHelper (Multiple n x) = replicate n x
