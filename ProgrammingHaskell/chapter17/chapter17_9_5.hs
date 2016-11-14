import Control.Applicative
import Data.Monoid (Monoid, (<>))
import Test.QuickCheck (Arbitrary, arbitrary, elements)
import Test.QuickCheck.Checkers (quickBatch, eq, (=-=), EqProp)
import Test.QuickCheck.Classes (applicative)

data Three' a b = Three' a b b deriving (Eq, Show)

instance Functor (Three' a ) where
        fmap f (Three' a b c) = Three' a (f b) (f c)

instance (Monoid a ) => Applicative (Three' a) where
        pure x = Three' mempty x x
        (<*>) (Three' a f f') (Three' a' b c) = Three' (a <> a') (f b) (f' c)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Three' a b) where
        arbitrary = do
                a <- arbitrary
                b <- arbitrary
                return (Three' a b b)

instance (Eq a, Eq b) => EqProp (Three' a b) where (=-=) = eq

-- quickBatch $ applicative (undefined :: Three' String (Int, Double, Char))
