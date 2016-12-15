import           Control.Monad

meh :: Monad m => [a] -> (a -> m b) -> m [b]
meh [] _       = pure []
meh (x : xs) f = (:) <$> f x <*> meh xs f

flipType :: (Monad m) => [m a] -> m [a]
flipType xs = meh xs (join . pure )
