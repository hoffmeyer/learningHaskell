
-- 1)
---- a)
stops  = "pbtdkg"
vowels = "aeiou"

stopVowStops = [(x,y,z) | x <- stops, y <- vowels, z <- stops]

---- b)
pVowStops = filter (\(x, y, z) -> x == 'p')  stopVowStops

---- c)
nouns = ["cat", "hat", "table", "beer", "lake", "toilet", "hipster", "beard", "nose", "book"]
verbs = ["eats", "drops", "hates", "loves", "kisses", "runs", "needs", "hears"]

sentences = [(x,y,z) | x <- nouns, y <- verbs, z <- nouns]

-- 2)
-- It finds the average word length of a sentence
seekritFunc :: String -> Int
seekritFunc x =
  div (sum (map length (words x)))
      (length (words x))

-- 3)
seekritFuncFrac x =
  (fromIntegral (sum (map length (words x)))) / (fromIntegral (length (words x)))
