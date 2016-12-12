import Control.Monad

meh :: Monad m => [a] -> (a -> m b) -> m [b]

