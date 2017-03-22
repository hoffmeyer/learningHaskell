altMap :: (a -> b) -> (a -> b) -> [a] -> [b]
altMap _ _ []     = []
altMap f g (x:xs) = f x : altMap' f g xs

altMap' :: (a -> b) -> (a -> b) -> [a] -> [b]
altMap' _ _ []     = []
altMap' f g (x:xs) = g x : altMap f g xs

luhnDouble :: Int -> Int
luhnDouble x = if d > 9 then d - 9 else d
        where
                d = x * 2

luhn :: [Int] -> Bool
luhn xs = sum (altMap id luhnDouble $ reverse xs) `mod` 10 == 0
