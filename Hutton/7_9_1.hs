mapAndFilter :: [a] -> (a -> Bool) -> (a -> b) -> [b]
mapAndFilter xs p f = map f (filter p xs)
