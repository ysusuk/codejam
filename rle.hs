module Rle where

import Data.List
import Data.Char

rle :: String -> String
rle str = unwords $ map (\letters -> show (length letters) ++ [(letters !! 0)]) (groupBy (==) str)

rleInverse :: String -> String
rleInverse str = filter (/= ' ') (unwords (map (\t -> replicate (digitToInt (t !! 0)) (t !! 1)) (words str)))
