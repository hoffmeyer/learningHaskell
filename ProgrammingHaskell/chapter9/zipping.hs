myZip [] _ = []
myZip  _ [] = []
myZip  (x:xs) (y:ys) = (x, y) : myZip xs ys

myZipWith _ [] _ = []
myZipWith _ _ [] = []
myZipWith f (x:xs) (y:ys) = (f x y) : myZipWith f xs ys

myZip2 = myZipWith (\x y -> (x, y)) 

