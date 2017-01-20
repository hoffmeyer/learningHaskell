merge :: Ord a => [a] -> [a] -> [a]
merge xs [] = xs
merge [] ys = ys
merge xs@(x:xs') ys@(y:ys')
  | x < y = x : merge xs' ys
  | x > y = y : merge xs ys'
