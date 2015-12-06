myCompare :: (Ord a) => a -> a -> Ordering
a `myCompare` b
    | a > b     = GT
    | a == b    = EQ
    | otherwise = LT

bmiTell :: (RealFloat a) => a -> a -> String
bmiTell weight height
    | bmi <= skinny = "Du er bare for tynd."
    | bmi <= normal = "Du er da rimelig normal."
    | bmi <= fat = "Du er fed, skal du ikke tabe dig lidt?"
    | otherwise   = "Du er meeeeega fed mand!"
    where   bmi = weight / height ^ 2
            skinny = 18.5
            normal = 25.0
            fat = 30.0

