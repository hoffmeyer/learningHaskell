module FunctionWithLet where

printInc2 :: Int -> IO()
printInc2 n = let
  plusTwo = n + 2
  in print plusTwo
