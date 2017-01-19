a2 :: [a] -> a
a2 = head . tail . tail

b2 :: [a] -> a
b2 xs = xs !! 2

c2 :: [a] -> a
c2 (x : (y : (z : zs))) = z
