import Data.Time

data DatabaseItem = DbString String
                  | DbNumber Integer
                  | DbDate UTCTime
                  deriving (Eq, Ord, Show)

theDatabase :: [DatabaseItem]
theDatabase =
  [ DbDate (UTCTime
             ( fromGregorian 1911 5 1)
             ( secondsToDiffTime 34123))
  , DbString "Hello, world!"
  , DbNumber 22
  , DbDate (UTCTime
             (fromGregorian 1921 5 1)
             ( secondsToDiffTime 34123))
  , DbNumber 12
  ]

filterDbDate :: [DatabaseItem] -> [UTCTime]
filterDbDate xs = foldr onlyDates [] xs
  where
          onlyDates (DbDate x) xs = x:xs
          onlyDates _ xs = xs

filterDbNumber :: [DatabaseItem] -> [Integer]
filterDbNumber xs = foldr onlyNumbers [] xs
  where
          onlyNumbers (DbNumber x) xs = x:xs
          onlyNumbers _ xs = xs

mostRecent :: [DatabaseItem] -> UTCTime
mostRecent = maximum . filterDbDate

sumDb :: [DatabaseItem] -> Integer
sumDb xs = foldr (+) 0 (filterDbNumber xs)

avgDb :: [DatabaseItem] -> Double
avgDb xs = fromIntegral ( sumDb xs) / fromIntegral( length $ filterDbNumber xs)
