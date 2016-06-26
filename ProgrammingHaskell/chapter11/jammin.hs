module Jammin where

import Data.List

data Fruit =
          Peach
        | Plum
        | Apple
        | Blackberry
        deriving (Ord, Eq, Show)

data JamJars = 
        Jam { fruit :: Fruit
                , num :: Int }
                deriving (Ord, Eq, Show)

row1 = Jam Peach 1
row2 = Jam Plum 19
row3 = Jam Apple 11
row4 = Jam Blackberry 7
row5 = Jam Apple 4
row6 = Jam Plum 1

allJam = [row1, row2, row3, row4, row5, row6]

numJars = map num allJam

totalJamJars = sum numJars

mostRow = foldr1  (\x y -> if  num x > num y then x else y) allJam

compareKind (Jam k _) (Jam k' _) = compare k k'

sortedJams = sortBy compareKind allJam

equalKind (Jam k _) (Jam k' _) = k == k'

groupJams = groupBy equalKind sortedJams
