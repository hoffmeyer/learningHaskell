-- 1)

tensdigit :: Integral a => a -> a
tensdigit x = d
  where xLast = x `div` 10
        d     = xLast `mod` 10

tensdigitA :: Integral a => a -> a
tensdigitA x = d
  where xLast = divMod x 10
        d     = fst xLast `mod` 10

hunsD x = d2
  where d  = divMod x 100
        d2 = fst d `mod` 10

-- 2)
foldBool :: a -> a -> Bool -> a
foldBool x y v
  | v = x
  | otherwise = y

foldBool2 :: a -> a -> Bool -> a
foldBool2 x y v  = case v of
  True -> x
  otherwise -> y

foldBool3 :: a -> a -> Bool -> a
foldBool3 x y True = x
foldBool3 x y False = y

-- 3)
g :: (a -> b) -> (a, c) -> (b, c)
g f (a, c) = (f a, c)
