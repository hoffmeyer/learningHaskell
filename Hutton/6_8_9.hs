import Prelude hiding (sum, take, last)

sum :: Num a => [a] -> a
sum [a] = a
sum (x:xs) = x + sum xs

take :: Int -> [a] -> [a]
take 0 xs = []
take n (x:xs) = x : take (n - 1) xs

last :: [a] -> a
last [x] = x
last (x:xs) = last xs
