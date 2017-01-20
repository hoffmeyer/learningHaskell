merge :: Ord a => [a] -> [a] -> [a]
merge xs [] = xs
merge [] ys = ys
merge xs@(x:xs') ys@(y:ys')
  | x < y = x : merge xs' ys
  | x > y = y : merge xs ys'

msort :: Ord a => [a] -> [a]
msort [a] = [a]
msort xs = merge (msort $ fst halves) (msort $ snd halves)
        where
                halves = halve xs

halve :: [a] -> ([a], [a])
halve xs = ( take h xs, drop h xs)
        where h = div  (length xs) 2
