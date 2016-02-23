-- 1)
functionC x y = case x > y of
  True -> x
  False -> y

-- 2)
ifEvenAdd2 n = case even n of
  True -> n + 2
  False -> n

-- 1)
nums x =
  case compare x 0 of
    LT -> -1
    GT -> 1
    _ -> 0

-- higher order functions exercises
dodgy :: Num a => a -> a -> a
dodgy x y = x + y * 10
oneIsOne = dodgy 1
oneIsTwo = flip dodgy 2

-- 1) 1
-- 2) 11
-- 3) 22
-- 4) 21
-- 5) 12
-- 6) 11
-- 7) 21
-- 8) 21
-- 9) 22
-- 10) 31
-- 11) 23

-- guards exercises
avgGrade :: (Fractional a, Ord a) => a -> Char
avgGrade x
  | y >= 0.9  = 'A'
  | y >= 0.8  = 'B'
  | y >= 0.7  = 'C'
  | y >= 0.59 = 'D'
  | y <  0.59 = 'F'
  where y = x / 100

-- 1)
-- No good it will always be caught on the otherwise branch

-- 2)
-- No good the branches needs to be in order in the way the comparisons are done

-- 3)
pal xs
  | xs == reverse xs = True
  | otherwise        = False

-- answer is b

-- 4)
-- lists implementing Eq

-- 5)
pal :: Eq a => [a] -> Bool

-- 6)
numbers x
  | x <  0 = -1
  | x == 0 = 0
  | x >  0 = 1

-- answer is c

-- 7)
-- Num Ord

-- 8)
numbers :: (Num a1, Num a, Ord a) => a -> a1
