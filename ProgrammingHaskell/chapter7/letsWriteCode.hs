-- 1)

tensdigit :: Integral a => a -> a
tensdigit x = d
  where xLast = x `div` 10
        d     = xLast `mod` 10

tensdigitA :: Integral a => a -> a
tensdigitA x = d
  where xLast = divMod x 10
        d     = fst xLast `mod` 10
