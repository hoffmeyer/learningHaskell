module Palindrome where

import Control.Monad
import System.Exit (exitSuccess)
import Data.Char (toLower, isLetter)

cleanup :: String -> String
cleanup as = map toLower (filter isLetter as)

palindrome :: IO ()
palindrome = forever $ do
        line1 <- getLine
        let cleanLine = cleanup line1
        if cleanLine == reverse cleanLine
           then putStrLn "It's a palindrome!"
           else do 
                  putStrLn "Nope!"
                  exitSuccess
