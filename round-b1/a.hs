import Data.List
import Data.Ord

data TestCase = TestCase String

main :: IO()
main = do
  contents <- getContents
  let lns = tail . lines $ contents
      testCasses = transform lns
      outs = map process testCasses
  putStr $ unlines (map appendCaseAndNum $ zip [1..] outs)

appendCaseAndNum :: (Int, String) -> String
appendCaseAndNum (n, str) = "Case #" ++ show n ++ ": " ++ str

transform :: [String] -> [TestCase]
transform = map TestCase

process :: TestCase -> String
process (TestCase test) =
  let
    matched = match test
    numbersInLetter = (phoneNumber ((snd (head matched)) : []) (length test) (tail matched))
  in
    concat (sort (map transformNumbersInLetterToNumbers numbersInLetter))

transformNumbersInLetterToNumbers "ZERO" = "0"
transformNumbersInLetterToNumbers "ONE" = "1"
transformNumbersInLetterToNumbers "TWO" = "2"
transformNumbersInLetterToNumbers "THREE" = "3"
transformNumbersInLetterToNumbers "FOUR" = "4"
transformNumbersInLetterToNumbers "FIVE" = "5"
transformNumbersInLetterToNumbers "SIX" = "6"
transformNumbersInLetterToNumbers "SEVEN" = "7"
transformNumbersInLetterToNumbers "EIGHT" = "8"
transformNumbersInLetterToNumbers "NINE" = "9"

phoneNumber :: [String] -> Int -> [(Int, String)] -> [String]
phoneNumber nums n [] = nums
phoneNumber nums n [x]
  | (length (concat nums)) == n = nums
  | otherwise = nums ++ [(snd x)]
phoneNumber nums n (x:xs)
  | (length (concat nums)) == n = nums
  | (length (nums ++ [(snd x)])) == n = nums ++ [(snd x)]
  | otherwise = phoneNumber (nums ++ [(snd x)]) n xs

numbersInLetter = ["ZERO", "ONE", "TWO", "THREE", "FOUR", "FIVE", "SIX", "SEVEN", "EIGHT", "NINE"]

anotherMatch :: String -> String -> Bool
anotherMatch str number =
  let
    sortedNum = sort number
    forThree = (length (filter (== 'E') str)) >= 2
    numInLToBool num = map (\x -> isSubsequenceOf (x:[]) str) num
    res = and (numInLToBool sortedNum)
  in
    if number == "THREE"
      then res && forThree
      else
        res

match :: String -> [(Int, String)]
match str =
  let
    filtered = filter (\x -> anotherMatch str x) numbersInLetter
    numbersWithLetter = map (\x -> filter (isSubsequenceOf $ x : []) filtered) str
    grouped = (groupBy (==) (sort (concat numbersWithLetter)))
    matched = reverse (sortBy (comparing fst) (zipWith (\a b -> (length a, head a)) grouped grouped))
  in
    matched

readInts :: String -> [Int]
readInts str = map read $ words str
