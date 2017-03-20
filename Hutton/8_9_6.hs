instance Eq a => Eq (Maybe a) where
(==) :: a -> a -> Bool
(==) (Just x) (Just y) = x == y
(==) Nothing Nothing = True
(==) _ _ = False
