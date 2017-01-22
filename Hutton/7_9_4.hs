dec2int :: [Int] -> Int
dec2int = foldl (\b a -> b * 10 + a ) 0
