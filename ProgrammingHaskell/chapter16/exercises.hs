{-# LANGUAGE FlexibleInstances #-}
module Exercises where

import Test.QuickCheck
import Test.QuickCheck.Function

a = fmap (+1) $ read "[1]" :: [Int]

b = (fmap . fmap) (++ "lol") (Just ["Hi,", "Hello"])

c = fmap (*2) (\x -> x - 2)

d = fmap ((return '1' ++) . show) (\x -> [x, 1..3])

-- 1
newtype Identity a = Identity a deriving (Eq, Show)

instance Functor Identity where
        fmap f (Identity a) = Identity ( f a )

instance Arbitrary a => Arbitrary  (Identity a) where
        arbitrary = do
                a <- arbitrary
                return (Identity a)

-- 2
data Pair a = Pair a a deriving (Eq, Show)

instance Functor Pair where
        fmap f (Pair x y) = Pair (f x ) ( f y )

instance Arbitrary a => Arbitrary  (Pair a) where
        arbitrary = do
                a <- arbitrary
                return (Pair a a)

-- 3
data Two a b = Two a b deriving (Eq, Show)

instance Functor (Two a) where
        fmap f (Two a b) = Two a (f b)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
        arbitrary = do
                a <- arbitrary
                b <- arbitrary
                return (Two a b)

-- 4
data Three a b c = Three a b c deriving (Eq, Show)

instance Functor (Three a b) where
        fmap f (Three a b c) = Three a b (f c)

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
        arbitrary = do
                a <- arbitrary
                b <- arbitrary
                c <- arbitrary
                return (Three a b c)


-- 5
data Three' a b = Three' a b b deriving (Eq, Show)

instance Functor (Three' a) where
        fmap f (Three' x y z) = Three' x (f y) (f z)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Three' a b) where
        arbitrary = do
                a <- arbitrary
                b <- arbitrary
                return (Three' a b b)

-- 6
data Four a b c d = Four a b c d deriving (Eq, Show)

instance Functor (Four a b c)  where
        fmap f (Four a b c d) = Four a b c (f d)

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) => Arbitrary (Four a b c d) where
        arbitrary = do
                a <- arbitrary
                b <- arbitrary
                c <- arbitrary
                d <- arbitrary
                return (Four a b c d)

-- 7
data Four' a b = Four' a a a b deriving (Eq, Show)

instance Functor (Four' a) where
        fmap f (Four' a b c d) = Four' a b c (f d)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Four' a b) where
        arbitrary  = do
                a <- arbitrary
                b <- arbitrary
                return (Four' a a a b)

-- 8
data Trivial = Trivial 
-- No cant do functor for this

functorIdentity :: (Functor f, Eq (f a)) => f a -> Bool
functorIdentity f = fmap id f == f

main :: IO () 
main = do 
        quickCheck $ \x -> functorIdentity (x :: [Identity Int])
        quickCheck $ \x -> functorIdentity (x :: [Pair Int])
        quickCheck $ \x -> functorIdentity (x :: [Two Int Int])
        quickCheck $ \x -> functorIdentity (x :: [Three Int String Int])
        quickCheck $ \x -> functorIdentity (x :: [Three' Int Int])
        quickCheck $ \x -> functorIdentity (x :: [Four Int Int String Bool])
        quickCheck $ \x -> functorIdentity (x :: [Four' Int Bool])

-- Possibly

data Possibly a = LolNope | Yeppers a deriving (Eq, Show)

instance Functor Possibly where
        fmap f LolNope = LolNope
        fmap f (Yeppers a) = Yeppers (f a)

-- Sum
data Sum a b = First a 
             | Second b deriving (Eq, Show)

instance Functor (Sum a) where
        fmap f (First a) = First a 
        fmap f (Second b) = Second (f b)


-- Concluding exercises
-- 1 No
-- 2 yes
-- 3 yes
-- 4 yes
-- 5 no

-- 1
data Sum' b a = First' a
             | Second' b deriving (Eq, Show)

instance Functor (Sum' e) where
        fmap f (First' a) = First' (f a)
        fmap f (Second' b) = Second' b

-- 2
data Company a c b = DeepBlue a c
                   | Something b

instance Functor (Company e e') where
        fmap f (Something b) = Something (f b)
        fmap _ (DeepBlue a c) = DeepBlue a c

-- 3
data More b a = L a b a 
              | R b a b
              deriving (Eq, Show)

instance Functor (More x) where
        fmap f (L a b a') = L (f a) b (f a')
        fmap f (R b a b') = R b (f a) b'

-- 1
data Quant a b = Finance 
               | Desk a
               | Bloor b
               deriving (Eq, Show)

instance Functor (Quant a) where
        fmap f (Bloor b) = Bloor (f b)
        fmap _ Finance = Finance 
        fmap _ (Desk a) = Desk a

--2
data K a b = K a deriving (Eq, Show)

instance Functor (K a) where
        fmap f (K a) = K a

-- 3
newtype Flip f a b = Flip (f b a) deriving (Eq, Show)

instance Functor (Flip K a) where
        fmap f (Flip (K a)) = Flip $ K (f a)

-- 4
data EvilGoateeConst a b = GoatyConst b deriving (Eq, Show)

instance Functor (EvilGoateeConst a) where
        fmap f (GoatyConst b) = GoatyConst (f b)

-- 5
data LiftItOut f a = LiftItOut (f a) deriving (Eq, Show)

instance Functor f => Functor (LiftItOut f) where
        fmap f (LiftItOut fa) = LiftItOut (fmap f fa)

--6
data Parappa f g a = DaWrappa (f a) (g a) deriving (Eq, Show)

instance (Functor f, Functor g) => Functor (Parappa f g) where
        fmap f (DaWrappa fa ga) = LiftItOut (fmap f fa) (fmap f ga)
