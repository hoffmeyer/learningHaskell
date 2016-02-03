myAbs :: Integer -> Integer
myAbs x = if x < 0 then x * (-1) else x

f :: (a,b) -> (c,d) -> ((b,d), (a,c))
f (a,b) (c,d) = ((b,d), (a,c))

x = (+)

f' xs = x w  1
  where w = length xs

id = \ x -> x
myHead = \ (x : xs) -> x

myFst (a,b) = a
