data TestCase = TestCase [Int] [String]

main :: IO()
main = do
  contents <- getContents
  let lns = tail . lines $ contents
      testCasses = transform $ group (*1) lns
      outs = map process testCasses
  putStr $ unlines (map appendCaseAndNum $ zip [1..] outs)

appendCaseAndNum :: (Int, String) -> String
appendCaseAndNum (n, str) = "Case #" ++ show n ++ ": " ++ str

group :: (Int -> Int) -> [String] -> [[String]]
group _ [] = []
group f xs =
  let
    rowsAndCols = readInts (head xs)
    rows = 1 + (f $ rowsAndCols !! 0)
  in
    take rows xs : group f (drop rows xs)

transform :: [[String]] -> [TestCase]
transform =
  let
    createTestCase (meta:test) = TestCase (readInts meta) test
  in map createTestCase

process :: TestCase -> String
process testCase =
  case testCase of
    TestCase meta test ->
        show meta ++ show test

readInts :: String -> [Int]
readInts str = map read $ words str
