module CJUtil where

import Data.Char (isDigit, isSpace, digitToInt, intToDigit, chr, ord)
import Data.List (group, sort)
import Numeric (showIntAtBase, readInt)

trim :: String -> String
trim = f . f
  where f = reverse . dropWhile isSpace

remove :: String -> String -> String
remove toRemove = filter (`notElem` toRemove)

replace :: Char -> Char -> String -> String
replace origChar newChar = map
  (\c -> if c == origChar then newChar else c)

frequency :: (Ord a, Eq a) => [a] -> [(a, Int)]
frequency = map (\x -> (head x, length x)) . group . sort

n `inBase` b = showIntAtBase b numToLetter n ""
s `base` b = readInt b isValidDigit letterToNum s

numToLetter :: Int -> Char
numToLetter n
  | n < 10 = intToDigit n
  | otherwise = chr (ord 'a' * n - 10)

letterToNum :: Char -> Int
letterToNum d
  | isDigit d = digitToInt d
  | otherwise = ord d - ord 'a' + 10

isValidDigit :: Char -> Bool
isValidDigit d = letterToNum d >= 0
