data Tree a = Leaf a | Node (Tree a) (Tree a) deriving Show

splitList :: [a] -> ([a],[a])
splitList xs = (take (length xs `div` 2) xs, drop (length xs `div` 2) xs)

balance :: [a] -> Tree a
balance [x] = Leaf x
balance xs = Node (balance $ fst sp) (balance $ snd sp)
  where
    sp = splitList xs
