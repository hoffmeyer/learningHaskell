{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}

module GoatLogic where

class TooMany a where
        tooMany :: a -> Bool

instance TooMany Int where
        tooMany n = n > 42

instance TooMany (Int, String) where
        tooMany (n,s) = n > 42

instance TooMany (Int, Int) where 
        tooMany (n,m) = (n + m) > 42

instance (Num a, TooMany a) => TooMany (a, a) where
          tooMany (a, a') = tooMany (a + a')
