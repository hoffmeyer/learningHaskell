data Tree a = Leaf a | Node (Tree a) (Tree a)

countLeaves :: Tree a -> Integer
countLeaves (Leaf a) = 1
countLeaves (Node l r) = countLeaves l + countLeaves r

balanced :: Tree a -> Bool
balanced (Leaf a) = True
balanced (Node l r) = abs (countLeaves l - countLeaves r) <= 1
