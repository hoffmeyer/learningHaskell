module Arith4 where

-- id :: a -> a
-- id x = x

roundTrip :: (Show a, Read a) => a -> a
roundTrip a = read (show a)

roundTripPointFree :: (Show a, Read a) => a -> a
roundTripPointFree = read . show

roundTripPointFree2 :: (Show a, Read b) => a -> b
roundTripPointFree2 = read . show

main = do
  print (roundTrip 4)
  print (roundTripPointFree 4)
  print ((roundTripPointFree2 4) :: Int )
  print (id 4)
