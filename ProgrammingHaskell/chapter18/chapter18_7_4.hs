import Prelude hiding (Left, Right)
import Data.Monoid
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

data List a = Nil
            | Cons a (List a)
            deriving (Eq, Show) 

instance Monoid (List a) where
        mempty = Nil
        mappend a Nil = a
        mappend Nil a = a
        mappend (Cons x xs) ys = Cons x $ xs `mappend` ys

instance Functor List where
  fmap f Nil            = Nil
  fmap f (Cons a  list) = Cons (f a) (fmap f list)

take' :: Int -> List a -> List a
take' 0 _ = Nil
take' _ Nil = Nil
take' n (Cons x xs) = Cons x (take' (n - 1) xs)

flatMap :: (a -> List b) -> List a -> List b
flatMap f as = concat' $ fmap f as

concat' :: List (List a) -> List a
concat' = fold append Nil

append :: List a -> List a -> List a
append Nil ys         = ys
append (Cons x xs) ys = Cons x $ xs `append` ys

fold :: (a -> b -> b) -> b -> List a -> b
fold _ b Nil        = b
fold f b (Cons h t) = f h (fold f b t)

instance Applicative List where
  pure a = Cons a Nil
  Nil <*> _ = Nil
  _ <*> Nil = Nil
  (<*>) (Cons f b) ca = fmap f ca <> (b <*> ca)

instance Monad List where
        return = pure
        (>>=) Nil _ = Nil
        (>>=) (Cons a as) f = f a <> (as >>= f )

instance Arbitrary a => Arbitrary (List a) where
        arbitrary = do
                x <- arbitrary
                y <- arbitrary
                frequency [ (1, return Nil)
                          , (3, return (Cons x y))]

instance Eq a => EqProp (List a) where
        xs =-= ys = xs' `eq` ys'
                where xs' = take' 3000 xs
                      ys' = take' 3000 ys

main = do
        let trigger = undefined :: List (Int, String, Int)
        quickBatch $ functor trigger
        quickBatch $ applicative trigger
        quickBatch $ monad trigger
