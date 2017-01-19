expi :: Int -> Int -> Int
expi m 0 = m
expi m n = m * expi m (n - 1)
