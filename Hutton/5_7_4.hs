replicates :: Int -> a -> [a]
replicates n x = [x | _ <- [1..n]]
