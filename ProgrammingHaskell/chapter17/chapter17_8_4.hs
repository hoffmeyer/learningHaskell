import Control.Applicative
import Data.Monoid
import Test.QuickCheck hiding (Success, Failure)
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

data Validation e a = Failure e
                    | Success a
                    deriving (Eq, Show)

instance Functor (Validation e) where
        fmap _ (Failure e) = Failure e
        fmap f (Success a) = Success (f a)

instance (Arbitrary a, Arbitrary e) => Arbitrary (Validation e a) where
        arbitrary = do
                a <- arbitrary
                e <- arbitrary
                frequency [ (1, return (Failure e))
                          , (1, return (Success a))]

instance (Eq a, Eq e) => EqProp (Validation a e) where (=-=) = eq

instance Monoid e => Applicative (Validation e) where
        pure = Success
        (Failure e) <*> (Success _) = Failure e
        (Failure e) <*> (Failure f) = Failure ( e <> f) 
        (Success f) <*> r = fmap f r

-- quickBatch $ applicative (Failure "ohno")
