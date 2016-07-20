module Phone where

import Prelude hiding (lookup)
import Data.Map hiding (map)
import Data.List
import Data.Function (on)

-- validButtons = "1234567890*#"
type Digit = Char

-- valid presses: 1 and up
type Presses = Int

--data Letter = Letter Map

data DaPhone = DaPhone (Map Digit [(Digit, Presses)])

myPhone = DaPhone (fromList[('1', [('1', 1)])
                           ,('a', [('2', 1)])
                           ,('b', [('2', 2)])
                           ,('c', [('2', 3)])
                           ,('2', [('2', 4)])
                           ,('d', [('3', 1)])
                           ,('e', [('3', 2)])
                           ,('f', [('3', 3)])
                           ,('3', [('3', 4)])
                           ,('g', [('4', 1)])
                           ,('h', [('4', 2)])
                           ,('i', [('4', 3)])
                           ,('4', [('4', 4)])
                           ,('j', [('5', 1)])
                           ,('k', [('5', 2)])
                           ,('l', [('5', 3)])
                           ,('5', [('5', 4)])
                           ,('m', [('6', 1)])
                           ,('n', [('6', 2)])
                           ,('o', [('6', 3)])
                           ,('6', [('6', 4)])
                           ,('p', [('7', 1)])
                           ,('q', [('7', 2)])
                           ,('r', [('7', 3)])
                           ,('s', [('7', 4)])
                           ,('7', [('7', 5)])
                           ,('t', [('8', 1)])
                           ,('u', [('8', 2)])
                           ,('v', [('8', 3)])
                           ,('8', [('8', 4)])
                           ,('w', [('9', 1)])
                           ,('x', [('9', 2)])
                           ,('y', [('9', 3)])
                           ,('z', [('9', 4)])
                           ,('9', [('9', 5)])
                           ,('A', [('*', 1), ('2', 1)])
                           ,('B', [('*', 1), ('2', 2)])
                           ,('C', [('*', 1), ('2', 3)])
                           ,('D', [('*', 1), ('3', 1)])
                           ,('E', [('*', 1), ('3', 2)])
                           ,('F', [('*', 1), ('3', 3)])
                           ,('G', [('*', 1), ('4', 1)])
                           ,('H', [('*', 1), ('4', 2)])
                           ,('I', [('*', 1), ('4', 3)])
                           ,('J', [('*', 1), ('5', 1)])
                           ,('K', [('*', 1), ('5', 2)])
                           ,('L', [('*', 1), ('5', 3)])
                           ,('M', [('*', 1), ('6', 1)])
                           ,('N', [('*', 1), ('6', 2)])
                           ,('O', [('*', 1), ('6', 3)])
                           ,('P', [('*', 1), ('7', 1)])
                           ,('Q', [('*', 1), ('7', 2)])
                           ,('R', [('*', 1), ('7', 3)])
                           ,('S', [('*', 1), ('7', 4)])
                           ,('T', [('*', 1), ('8', 1)])
                           ,('U', [('*', 1), ('8', 2)])
                           ,('V', [('*', 1), ('8', 3)])
                           ,('W', [('*', 1), ('9', 1)])
                           ,('X', [('*', 1), ('9', 2)])
                           ,('Y', [('*', 1), ('9', 3)])
                           ,('Z', [('*', 1), ('9', 4)])
                           ,(' ', [('0', 1)])
                           ,('0', [('0', 2)])
                           ,('+', [('0', 3)])
                           ,('.', [('#', 1)])
                           ,(',', [('#', 2)])
                           ])

convo :: [String]
convo =
        ["Wanna play 20 questions",
         "Ya",
         "U 1st haha",
         "Lol ok. Have u ever tasted alcohol lol",
         "Lol ya",
         "Wow ur cool haha. Ur turn",
         "Ok. Do u think I am pretty Lol",
         "Lol ya",
         "Haha thanks just making sure rofl ur turn"]


reverseTaps :: DaPhone -> Char -> [(Digit, Presses)]
reverseTaps (DaPhone p) c = p ! c

cellPhonesDead :: DaPhone -> String -> [(Digit, Presses)]
cellPhonesDead p = concatMap (reverseTaps p)

typeLetters :: (Digit, Presses) -> String
typeLetters (d, p) = replicate p d

typeAllTexts :: [String] -> [String]
typeAllTexts = map (concatMap typeLetters . cellPhonesDead myPhone)

totNumDigits :: [String] -> [Int]
totNumDigits texts = map length (typeAllTexts texts)

mostPopularLetter :: String -> Char
mostPopularLetter s = head $ maximumBy (compare `on` length) $ group . sort $ s
