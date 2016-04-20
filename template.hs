module CJUtil where

import Data.Char (isSpace)
import Data.List (group, sort)

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
