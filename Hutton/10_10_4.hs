adder :: IO ()
adder = do putStr "How many numbers? "
           count <- readLn
           sum <- adder' count 0
           putStrLn $ "The total is: " ++ show sum

adder' :: Int -> Int -> IO Int
adder' 0 total = return total
adder' n total = do num <- readLn
                    adder' (n-1) (num + total)
