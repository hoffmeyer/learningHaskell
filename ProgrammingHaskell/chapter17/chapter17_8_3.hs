module Apl2 where

import           Control.Applicative
import           Data.Monoid
import           Test.QuickCheck
import           Test.QuickCheck.Checkers
import           Test.QuickCheck.Classes

data List a =
    Nil
  | Cons a (List a)
  deriving (Eq, Show)

take' :: Int -> List a -> List a
take' = undefined

instance Functor List where
  fmap f Nil            = Nil
  fmap f (Cons a  list) = Cons (f a) (fmap f list)

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
  Nil <*> as = Nil
  fs <*> Nil = Nil
  fs <*> as = flatMap (`fmap` as ) fs

newtype ZipList' a =
  ZipList' (List a)
  deriving (Eq, Show)

instance Eq a => EqProp (ZipList' a ) where
  xs =-= ys = xs' `eq` ys'
    where xs' = let (ZipList' l) = xs
                in take' 3000 l
          ys' = let (ZipList' l) = ys
                in take' 3000 l

instance Functor ZipList' where
  fmap f (ZipList' xs) = ZipList' $ fmap f xs

repeatList :: a -> List a
repeatList x = xs
  where xs = Cons x xs

zipListWith :: (a -> b -> c) -> List a -> List b -> List c
zipListWith _ Nil _                   = Nil
zipListWith _ _ Nil                   = Nil
zipListWith f (Cons a as) (Cons b bs) = Cons (f a b) (zipListWith f as bs)

instance Applicative ZipList' where
  pure x = ZipList' (repeatList x)
  ZipList' fs <*> ZipList' xs = ZipList' (zipListWith id fs xs)
