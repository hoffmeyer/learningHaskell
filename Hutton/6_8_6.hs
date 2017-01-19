import Prelude hiding (and, concat, replicate, (!!), elem)

-- a
and :: [Bool] -> Bool
and [x] = x
and (x:xs) 
  | x = and xs
  | otherwise = False

-- b
concat :: [[a]] -> [a]
concat [x] = x
concat (x:xs) = x ++ concat xs

-- c
replicate :: Int -> a -> [a]
replicate 0 _ = []
replicate n x = x : replicate (n - 1) x

-- d
(!!) :: [a] -> Int -> a
(!!) (x:xs) 0 = x
(!!) (x:xs) n = (!!) xs (n - 1)

-- e
elem :: Eq a => a -> [a] -> Bool
elem _ [] = False
elem n (x:xs)
  | n == x = True
  | otherwise = elem n xs
