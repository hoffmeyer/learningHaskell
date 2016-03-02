module EnumFromTo where

eftBool :: Bool -> Bool -> [Bool]
eftBool = eftHelp

eftOrd :: Ordering -> Ordering -> [Ordering]
eftOrd = eftHelp

eftInt :: Int -> Int -> [Int]
eftInt = eftHelp

eftChar :: Char -> Char -> [Char]
eftChar = eftHelp

eftHelp :: (Ord a, Enum a) => a -> a -> [a]
eftHelp x y
    | x > y = []
    | x == y = [x]
    | otherwise = x : eftHelp (succ x) y
