import Prelude hiding (map, filter)

map :: (a -> b) -> [a] -> [b]
map f = foldr (\a b -> f a : b) []

filter :: (a -> Bool) -> [a] -> [a]
filter f = foldr (\a b -> if f a then a : b else b) []
