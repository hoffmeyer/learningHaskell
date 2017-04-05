putBoard :: Board -> IO ()
putBoard b = sequence_ [ putRow x (b !! (x-1)) | x <- [1..5]]
