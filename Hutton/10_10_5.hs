adder :: IO ()
adder = do putStr "How many numbers? "
           count <- readLn
           nums <- sequence $ map (const readLn) [1..count]
           putStrLn $ "The total is: " ++ show (sum nums)
