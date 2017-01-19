euclid :: Int -> Int -> Int
euclid n m | n == m = n
  | n > m = euclid (n - m) m
  | otherwise = euclid n (m - n)
