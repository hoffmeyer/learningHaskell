module IntermissionEx where

myWords :: String -> Char -> [String]
myWords s c = go s c []
    where
        go :: String -> Char -> [String] -> [String]
        go s c ss
            | s == [] = ss
            | otherwise = go ( dropWhile (== c) $ dropWhile (/= c) s) c ( ss ++ [takeWhile (/= c) s])

firstSen  = "Tyger Tyger, burning bright\n"
secondSen = "In the forests of the night\n"
thirdSen  = "What immortal hand or eye\n"
fourthSen = "Could frame thy fearful symmetry?"

sentences = firstSen ++ secondSen
          ++ thirdSen ++ fourthSen

myLines :: String -> [String]
myLines s = myWords s '\n'

shouldEqual =
  [ "Tyger Tyger, burning bright"
  , "In the forests of the night"
  , "What immortal hand or eye"
  , "Could frame thy fearful symmetry?"
  ]

main :: IO ()
main =
  print $ "Are they equal? "
         ++ show (myLines sentences == shouldEqual)
