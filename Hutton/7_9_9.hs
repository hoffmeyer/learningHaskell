altMap :: (a -> b) -> (a -> b) -> [a] -> [b]
altMap _ _ []     = []
altMap f g (x:xs) = f x : altMap' f g xs

altMap' :: (a -> b) -> (a -> b) -> [a] -> [b]
altMap' _ _ []     = []
altMap' f g (x:xs) = g x : altMap f g xs
