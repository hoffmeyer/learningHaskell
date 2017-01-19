sumdown :: Int -> Int
sumdown n | n > 0  = n + sumdown (n - 1)
          | otherwise = 0
