a = (* 9) 6
b = head[(0,"doge"),(1,"kitteh")]
c = head[(0::Integer, "doge"),(1,"kitteh")]
d = if False then True else False
e = length [1,2,3,4,5]
f = (length [1,2,3,4]) > (length "TACOCAT")


x = 5
y = x + 5
w = y * 10
z y = y * 10
g = 4 / y


functionH :: (a,b) -> a
functionH (x,_) = x

functionC :: Ord a => a -> a -> Bool
functionC x y = if (x > y) then True else False

functionS :: (a,b) -> b
functionS (x,y) = y

i :: a -> a
i x = x

c' :: a -> b -> a
c' x y = x

c'' :: a -> b -> b
c'' x y = y

r :: [a] -> [a]
r = reverse

co :: (b -> c) -> (a -> b) -> (a -> c)
co f g x = f $ g x

af :: (a -> c) -> a -> a
af f x = x

af' :: (a -> b) -> a -> b
af' f = f
