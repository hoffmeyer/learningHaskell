import Data.Monoid

data Validation e a = Failure e
                    | Success a
                    deriving (Eq, Show)

instance Functor (Validation e) where
        fmap _ (Failure e) = Failure e
        fmap f (Success a) = Success (f a)

instance Monoid e => Applicative (Validation e) where
        pure = Success
        (Failure e) <*> (Success _) = Failure e
        (Failure e) <*> (Failure f) = Failure ( e <> f) 
        (Success f) <*> r = fmap f r
