module Exercises where

import Data.Semigroup
import Test.QuickCheck as Q

-- 1

data Trivial = Trivial deriving (Eq, Show)

instance Semigroup Trivial where
        _ <> _ = Trivial

instance Arbitrary Trivial where
        arbitrary = return Trivial

type TrivialAssoc = Trivial -> Trivial -> Trivial -> Bool

-- 2

newtype Identity a = Identity a deriving (Eq, Show)

instance Semigroup (Identity a) where
        x <> _ = x

instance Arbitrary a => Arbitrary  (Identity a) where
        arbitrary = do
                a <- arbitrary
                return (Identity a)

type IdentityAssoc = Identity String -> Identity String -> Identity String -> Bool

-- 3

data Two a b = Two a b deriving (Eq, Show)

instance Semigroup (Two a b) where
        x <> _ = x

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
        arbitrary = do
                a <- arbitrary
                b <- arbitrary
                return (Two a b)

type TwoAssoc = Two String Integer -> Two String Integer -> Two String Integer -> Bool

-- 4

data Three a b c = Three a b c deriving (Eq, Show)

instance Semigroup (Three a b c) where
        x <> _ = x

instance (Arbitrary a, Arbitrary b, Arbitrary c)  => Arbitrary (Three a b c) where
        arbitrary = do
                a <- arbitrary
                b <- arbitrary
                c <- arbitrary
                return (Three a b c)

type ThreeAssoc = Three String Int Bool -> Three String Int Bool -> Three String Int Bool -> Bool
-- Skipping 5 this is getting tedious

-- 6

newtype BoolConj = BoolConj Bool deriving (Eq, Show)

instance Semigroup BoolConj where
        (BoolConj x) <> (BoolConj y) = BoolConj( x && y)

instance Arbitrary BoolConj where
        arbitrary = do
                a <- arbitrary
                return (BoolConj a)

type BoolConjAssoc = BoolConj -> BoolConj -> BoolConj -> Bool

-- 7

newtype BoolDisj = BoolDisj Bool deriving (Eq, Show)

instance Semigroup BoolDisj where
        (BoolDisj x) <> (BoolDisj y) = BoolDisj( x || y)

instance Arbitrary BoolDisj where
        arbitrary = do
                a <- arbitrary
                return (BoolDisj a)

type BoolDisjAssoc = BoolDisj -> BoolDisj -> BoolDisj -> Bool

-- 8

data Or a b = Fst a
            | Snd b
            deriving (Eq, Show)

instance Semigroup (Or a b) where
        (Fst x) <> (Snd y) = Snd y
        (Fst x) <> (Fst y) = Fst y
        (Snd x) <> (Fst y) = Snd x
        (Snd x) <> (Snd y) = Snd x

instance (Arbitrary a, Arbitrary b) => Arbitrary (Or a b) where
        arbitrary = do
                a <- arbitrary
                b <- arbitrary
                frequency [ (1, return (Fst a))
                          , (1, return (Snd b))]

type OrAssoc = Or String Integer -> Or String Integer -> Or String Integer -> Bool

-- 9
newtype Combine a b = Combine { unCombine :: a -> b } 

instance Semigroup b => Semigroup (Combine a b) where
        (Combine f) <> (Combine g) = Combine (f <> g)

-- 10

newtype Comp a = Comp { unComp :: a -> a }

instance Semigroup (Comp a) where
        (Comp f) <> (Comp g) = Comp (f . g)

-- 11 
data Validation a b = Failure a 
                    | Success b
                    deriving (Eq, Show)

instance (Semigroup a, Semigroup b) => Semigroup (Validation a b) where
        (Exercises.Failure x) <> (Exercises.Failure y) = Exercises.Failure (x <> y)
        (Exercises.Success x) <> (Exercises.Success y) = Exercises.Success (x <> y)
        (Exercises.Failure x) <> _ = Exercises.Failure x
        _ <> (Exercises.Failure x) = Exercises.Failure x

instance (Arbitrary a, Arbitrary b) => Arbitrary (Validation a b) where
        arbitrary = do
                a <- arbitrary
                b <- arbitrary
                frequency [ (1, return (Exercises.Failure a))
                          , (1, return (Exercises.Success b))]

type ValidationAssoc = Validation String String -> Validation String String -> Validation String String -> Bool

-- 12

newtype AccumulateRight a b = 
        AccumulateRight (Validation a b) 
        deriving (Eq, Show)

instance Semigroup b => Semigroup (AccumulateRight a b) where
        (AccumulateRight x ) <> (AccumulateRight y) = AccumulateRight y

instance (Arbitrary a, Arbitrary b) => Arbitrary (AccumulateRight a b) where
        arbitrary = do
                a <- arbitrary
                b <- arbitrary
                frequency [ (1, return (AccumulateRight (Exercises.Failure a)))
                          , (1, return (AccumulateRight (Exercises.Success b)))]

type AccumulateRightAssoc = AccumulateRight String String -> AccumulateRight String String -> AccumulateRight String String -> Bool


semigroupAssoc :: (Eq m, Semigroup m) => m -> m -> m -> Bool
semigroupAssoc a b c = (a <> (b <> c)) == (a <> b) <> c


main :: IO ()
main = do
        quickCheck (semigroupAssoc :: TrivialAssoc)
        quickCheck (semigroupAssoc :: IdentityAssoc)
        quickCheck (semigroupAssoc :: TwoAssoc)
        quickCheck (semigroupAssoc :: ThreeAssoc)
        quickCheck (semigroupAssoc :: BoolConjAssoc)
        quickCheck (semigroupAssoc :: BoolDisjAssoc)
        quickCheck (semigroupAssoc :: OrAssoc)
        quickCheck (semigroupAssoc :: ValidationAssoc)
        quickCheck (semigroupAssoc :: AccumulateRightAssoc)
