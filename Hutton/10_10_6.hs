import           System.IO

getCh :: IO Char
getCh = do hSetEcho stdin False
           x <- getChar
           hSetEcho stdin True
           return x

readLine :: IO String
readLine = readLine' ""

readLine' :: String -> IO String
readLine' result = do x <- getCh
                      case x of
                        '\n' -> do putChar x
                                   return result
                        '\DEL' -> do putChar '\b'
                                     readLine' $ init result
                        _ -> do putChar x
                                readLine' (result ++ [x])

main :: IO ()
main = do x <- readLine
          putStrLn $ "The text was: " ++ x
