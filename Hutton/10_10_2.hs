putBoard :: Board -> IO ()
putBoard b = putBoard'' b 1

putBoard' :: Board -> Int -> IO ()
putBoard' [] _ = return ()
putBoard' (x:xs) n  = do putRow n x
                         putBoard'' xs (n+1)
