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

-- 13
encodeDirect :: Eq a => [a] -> [Elem a]
encodeDirect = map encodeHelper . group
    where
        encodeHelper [x] = Single x
        encodeHelper xs = Multiple (length xs) (head xs)

--14
dupli :: [a] -> [a]
dupli [] = []
dupli (x:xs) = x:x:dupli xs
dupli' list =  concat [[x,x] | x <- list]

--15
repli :: [a] -> Int -> [a]
repli xs n = concat [ replicate n x | x <- xs]
repli' xs n = concatMap (replicate n) xs

--16
dropEvery :: [a] -> Int -> [a]
dropEvery xs n = recur n 1 xs
    where
        recur :: Int -> Int -> [a] -> [a]
        recur _ _ [] = []
        recur n i (x:xs) = if i == n
            then recur n 1 xs
            else x:recur n (i+1) xs

--17
split :: [a] -> Int -> ([a],[a])
split xs n = ( take n xs, drop n xs)
split' xs n = splitHelper [] xs n
    where 
        splitHelper :: [a] -> [a] -> Int -> ([a], [a])
        splitHelper ys xs n  
            | length ys == n    = (ys, xs)
            | otherwise         = splitHelper (ys ++ [(head xs)]) (tail xs) n
split'' (x:xs) n | n > 0 = let (f,l) = split'' xs (n-1) in (x : f, l)
split'' xs _             = ([], xs)
