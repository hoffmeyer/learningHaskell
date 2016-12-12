import Control.Monad
j :: Monad m => m (m a) -> m a
j = join
