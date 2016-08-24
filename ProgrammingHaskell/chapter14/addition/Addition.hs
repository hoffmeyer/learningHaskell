module Addition where

import Test.Hspec
import Test.QuickCheck

dividedBy :: Integral a => a -> a -> (a, a)
dividedBy num denom = go num denom 0
        where go n d count
               | n < d = (count, n)
               | otherwise = go (n - d) d (count + 1)

multiplyBy :: (Eq a, Num a) => a -> a -> a
multiplyBy x 0 = 0
multiplyBy x y = x + multiplyBy x (y-1)

trivialInt :: Gen Int
trivialInt = return 1

oneThroughThree :: Gen Int
oneThroughThree = elements [1,2,2,2,2,3]

genBool :: Gen Bool
genBool = choose (False, True)

genBool' :: Gen Bool
genBool' = elements [False, True]

genOrdering :: Gen Ordering
genOrdering = elements [LT, EQ, GT]

genChar :: Gen Char
genChar = elements ['a'..'z']

genTuple :: (Arbitrary a, Arbitrary b) => Gen (a, b)
genTuple = do
        a <- arbitrary
        b <- arbitrary
        return (a, b)

genTriple :: (Arbitrary a, Arbitrary b, Arbitrary c) => Gen (a, b, c)
genTriple = do
        a <- arbitrary
        b <- arbitrary
        c <- arbitrary
        return (a, b, c)

genEither :: (Arbitrary a, Arbitrary b) => Gen (Either a b)
genEither = do
        a <- arbitrary
        b <- arbitrary
        elements [Left a, Right b]

genMaybe :: Arbitrary a => Gen (Maybe a)
genMaybe = do
        a <- arbitrary
        elements [Nothing, Just a]

genMaybe' :: Arbitrary a => Gen (Maybe a)
genMaybe' = do
        a <- arbitrary
        frequency [ (1, return Nothing)
                  , (3, return (Just a))]

prop_additionGreater :: Int -> Bool
prop_additionGreater x = x + 1 > x

runQc :: IO ()
runQc = quickCheck prop_additionGreater


main :: IO ()
main = hspec $ do
        describe "Addition" $ do
                it "15 divideed by 3 is 5" $ do
                        dividedBy 15 3 `shouldBe` (5,0)
                it "22 divided by 5 is 4 remainder 2" $ do
                        dividedBy 22 5 `shouldBe` (4,2)
                it "5 times 5 is 25" $ do
                        multiplyBy 5 5 `shouldBe` 25
                it "0 times 5 is 0" $ do
                        multiplyBy 0 5 `shouldBe` 0
                it "5 times 0 is 0" $ do
                        multiplyBy 5 0 `shouldBe` 0
                it "x + 1 is always greater than x" $ do
                        property $ \x -> x + 1 > (x :: Int)
