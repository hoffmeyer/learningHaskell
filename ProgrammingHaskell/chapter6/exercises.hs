-- 1
data TisAndInteger =
  TisAn Integer

instance Eq TisAndInteger where
  TisAn v == TisAn v' = v == v'

-- 2
data TwoIntegers =
  Two Integer Integer

instance Eq TwoIntegers where
  Two v w == Two v' w' = v == v' && w == w'

-- 3
data StringOrInt =
    TisAnInt   Int
  | TisAString String

instance Eq StringOrInt where
  (==) (TisAnInt v) (TisAnInt v') = v == v'
  (==) (TisAString v) (TisAString v') = v == v'
  (==) _ _ = False

-- 4
data Pair a =
  Pair a a

instance Eq a => Eq (Pair a) where
  (==) (Pair a b) (Pair a' b') = a == a' && b == b'

-- 5
data Tuple a b =
  Tuple a b

instance (Eq a, Eq b) => Eq (Tuple a b) where
  (==) (Tuple a b) (Tuple a' b') = a == a' && b == b'

-- 6
data Which a =
    ThisOne a
  | ThatOne a

instance Eq a => Eq (Which a) where
  (==) (ThisOne a) (ThisOne a') = a == a'
  (==) (ThatOne a) (ThatOne a') = a == a'
  (==) _ _ = False

-- 7
data EitherOr a b =
    Hello a
  | Goodbye b

instance (Eq a, Eq b) => Eq (EitherOr a b) where
  (==) (Hello a) (Hello a') = a == a'
  (==) (Goodbye b) (Goodbye b') = b == b'
  (==) _ _ = False
