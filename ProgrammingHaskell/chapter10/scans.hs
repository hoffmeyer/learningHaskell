
fibs = 1 : scanl (+) 1 fibs

fibsN x = fibs !! x

fibs20 = take 20 fibs

fibsLT100 = takeWhile (\x -> x < 100) fibs

factorial :: Integer -> Integer
factorial 0 = 1
factorial n = n * factorial  (n - 1)

factorialScan = scanl (*) 1 [2..]
