-- a)
addOneIfOdd n = case odd n of
  True -> f n
  False -> n
  where f = \n -> n + 1

-- b)
addFive = \ x y -> (if x > y then y else x) + 5

-- c
mflip f x y = f y x

-- Entermission exercises

-- 1)

-- a)
-- k :: (a, b) -> a
-- b)
--String, No
-- c
-- k3

-- 2)
f :: (a, b, c) -> (d, e, f) -> ((a, d), (c, f))
f (a, _, c) (d, _, f) = ((a, d), (c, f))
