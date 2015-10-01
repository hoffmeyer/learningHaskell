-- 1
myLast [] = error "No end for empty lists"
myLast [x] = x
myLast (_:xs) = myLast xs

-- 2
myButLast [] = error "No second last element for empty lists"
myButLast [_] = error "No second last element for lists of length 1"
myButLast [x,y] = x
myButLast (_:xs) = myButLast xs
