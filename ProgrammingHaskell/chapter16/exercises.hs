module Exercises where

import Test.QuickCheck
import Test.QuickCheck.Function

a = fmap (+1) $ read "[1]" :: [Int]

b = (fmap . fmap) (++ "lol") (Just ["Hi,", "Hello"])

c = fmap (*2) (\x -> x - 2)

d = fmap ((return '1' ++) . show) (\x -> [x, 1..3])

functorIdentitty :: (Functor f, Eq (f a)) => f a -> Bool
functorIdentitty f = fmap id f == f

newtype Identity a = Identity a deriving (Show)

instance Functor Identity where
        fmap f (Identity a) = Identity ( f a )

functorCompose :: (Eq (f c), Functor f) => f a -> Fun a b -> Fun b c -> Bool
functorCompose x (Fun _ f) (Fun _ g) = fmap (g . f) x == (fmap g . fmap f $ x)

type IntToInt = Fun Int Int

main :: IO () 
main = do 
        quickCheck $ \x -> functorIdentitty (x :: [Int])
        quickCheck (functorCompose :: [Int] -> IntToInt -> IntToInt -> Bool)
