import Control.Monad
l2 :: Monad m => (a -> b -> c) -> m a -> m b -> m c
l2 = liftM2

