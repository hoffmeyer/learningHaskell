luhnDouble :: Int -> Int
luhnDouble x = if d > 9 then d - 9 else d
        where
                d = x * 2

luhn :: Int -> Int -> Int -> Int -> Bool
luhn a b c d = added `mod` 10 == 0 
        where 
                added = (luhnDouble a ) + b + ( luhnDouble c ) + d 
