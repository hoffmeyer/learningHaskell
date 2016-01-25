module FunctionWithWhere where

printInc :: Int -> IO()
printInc n = print plusTwo
  where plusTwo = n + 2
