import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

data Nope a = NopeDotJpg deriving (Eq, Show)

instance Functor Nope where
        fmap _ NopeDotJpg = NopeDotJpg

instance Applicative Nope where
        pure _= NopeDotJpg
        NopeDotJpg <*> NopeDotJpg = NopeDotJpg

instance Monad Nope where
        return _ = NopeDotJpg
        NopeDotJpg >>= _ = NopeDotJpg

instance Arbitrary a => Arbitrary (Nope a) where
        arbitrary = return NopeDotJpg

instance Eq a => EqProp (Nope a) where (=-=) = eq

main = do
        let trigger = undefined :: Nope (Int, String, Int)
        quickBatch $ functor trigger
        quickBatch $ applicative trigger
        quickBatch $ monad trigger
