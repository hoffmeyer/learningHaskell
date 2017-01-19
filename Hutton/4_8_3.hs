safeTailA :: [a] -> [a]
safeTailA xs = if null xs then [] else tail xs

safeTailB :: [a] -> [a]
safeTailB xs 
  | null xs = []
  | otherwise = tail xs

safeTailC :: [a] -> [a]
safeTailC [] = []
safeTailC xs = tail xs
