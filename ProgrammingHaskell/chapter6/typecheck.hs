-- 1
data Person = Person Bool deriving Show

printPerson :: Person -> IO()
printPerson person = putStrLn (show person)

-- 2
data Mood = Blah
          | Woot deriving (Show, Eq)

settleDown x = if x == Woot
               then Blah
               else x

-- 3
-- a) Woot and Blah
-- b) compile error because 9 is not an instance of Woot or Blah
-- c) compile error because Mood does not derive Ord

-- 4
type Subject = String
type Verb = String
type Object = String

data Sentence =
  Sentence Subject Verb Object
  deriving (Eq, Show)

s1 = Sentence "dogs" "drool"
s2 = Sentence "Julie" "loves" "dogs"

-- Yes it typechecks, but s1 is a partially applied function

-- 5
data Rocks =
  Rocks String deriving (Eq, Show, Ord)

data Yeah =
  Yeah Bool deriving (Eq, Show, Ord)

data Papu =
  Papu Rocks Yeah
  deriving (Eq, Show, Ord)

-- 1)
phew = Papu (Rocks "chases") (Yeah True)
-- 2)
truth = Papu (Rocks "chomskydoz")
             (Yeah True)
-- 3)
equalityForall :: Papu -> Papu -> Bool
equalityForall p p' = p == p'

-- 4)
comparePapus :: Papu -> Papu -> Bool
comparePapus p p' = p > p'

-- Match the typechecks
-- 1) No it can not
i :: Num a => a
i = 1
-- 2) No it can not, needs Frational, or we need to skip the .0
f :: Float
f = 1.0

-- 3)
f' :: Fractional a => a
f' = 1.0
