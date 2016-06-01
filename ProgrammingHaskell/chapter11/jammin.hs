module Jammin where

data Fruit =
          Peach
        | Plum
        | Apple
        | Blackberry
        deriving (Ord, Eq, Show)

data JamJars = 
        JamJars { fruit :: Fruit
                , num :: Int }
                deriving (Ord, Eq, Show)


row1 = JamJars Peach 1
row2 = JamJars Plum 19
row3 = JamJars Apple 11
row4 = JamJars Blackberry 7
row5 = JamJars Apple 4
row6 = JamJars Plum 1

allJam = [row1, row2, row3, row4, row5, row6]

numJars = map num allJam

totalJamJars = sum numJars
