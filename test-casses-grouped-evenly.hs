data TestCase = TestCase3 Int String String

main :: IO()
main = do
  contents <- getContents
  let lns = tail . lines $ contents
      testCasses = transform $ group 3 lns
      outs = map process testCasses
  putStr $ unlines (map appendCaseAndNum $ zip [1..] outs)

appendCaseAndNum :: (Int, String) -> String
appendCaseAndNum (n, str) = "Case#" ++ show n ++ ": " ++ str

group :: Int -> [a] -> [[a]]
group _ [] = []
group n list
 | n > 0 = (take n list) : (group n (drop n list))
 | otherwise = error "Negarive n"

transform :: [[String]] -> [TestCase]
transform =
  let
    createTestCase gr = case gr of
      [a,b,c] -> TestCase3 (read a :: Int) b c
  in map createTestCase

process :: TestCase -> String
process testCase =
  case testCase of
    TestCase3 n str1 str2 ->
      let
        ints1 = readInts str1
        ints2 = readInts str2
      in
        show n ++ show ints1 ++ show ints2

readInts :: String -> [Int]
readInts str = map read $ words str
