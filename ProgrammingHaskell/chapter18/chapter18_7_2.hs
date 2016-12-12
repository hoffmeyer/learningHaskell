import Prelude hiding (Left, Right)
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

data PhhhbbtttEither b a = Left a
                         | Right b
                         deriving (Eq, Show)

instance Functor (PhhhbbtttEither b) where
        fmap f (Left a) =  Left (f a)
        fmap f (Right b) = Right b

instance Applicative (PhhhbbtttEither b) where
        pure = Left
        (<*>) _ (Right b) = Right b
        (<*>) (Right b) _ = Right b
        (<*>) (Left f) (Left a) = Left (f a)

instance Monad (PhhhbbtttEither b) where
        return = pure
        (>>=) (Right b) _ = Right b
        (>>=) (Left a) f = f a

instance ( Arbitrary a, Arbitrary b) => Arbitrary (PhhhbbtttEither a b) where
        arbitrary = do
                a <- arbitrary
                b <- arbitrary
                frequency [ (1, return (Left a))
                          , (1, return (Right b))]

instance (Eq a, Eq b) => EqProp (PhhhbbtttEither a b) where (=-=) = eq

main = do
        let trigger = undefined :: PhhhbbtttEither Int (Int, String, Int)
        quickBatch $ functor trigger
        quickBatch $ applicative trigger
        quickBatch $ monad trigger
