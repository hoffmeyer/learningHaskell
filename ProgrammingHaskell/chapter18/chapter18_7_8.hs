import Control.Monad
a :: Monad m => m a -> m (a -> b) -> m b
a = flip ap
