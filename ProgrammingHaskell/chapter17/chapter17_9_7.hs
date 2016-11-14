import Control.Applicative
import Data.Monoid (Monoid, (<>))
import Test.QuickCheck (Arbitrary, arbitrary, elements)
import Test.QuickCheck.Checkers (quickBatch, eq, (=-=), EqProp)
import Test.QuickCheck.Classes (applicative)

data Four' a b = Four' a a a b deriving (Eq, Show)

instance Functor (Four' a ) where
        fmap f (Four' a b c d) = Four' a b c (f d)

instance (Monoid a ) => Applicative (Four' a) where
        pure = Four' mempty mempty mempty
        (<*>) (Four' a b c f) (Four' a' b' c' d) = Four' (a <> a') (b <> b') (c <> c') (f d)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Four' a b) where
        arbitrary = do
                a <- arbitrary
                b <- arbitrary
                return (Four' a a a b)

instance (Eq a, Eq b) => EqProp (Four' a b) where (=-=) = eq

-- quickBatch $ applicative  (undefined :: Four' String (Int, Double, Char))
