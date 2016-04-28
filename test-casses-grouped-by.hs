data TestCase = TestCase [Int] [String]

main :: IO()
main = do
  contents <- getContents
  let lns = tail . lines $ contents
      testCasses = transform $ group lns
      outs = map process testCasses
  putStr $ unlines (map appendCaseAndNum $ zip [1..] outs)

appendCaseAndNum :: (Int, String) -> String
appendCaseAndNum (n, str) = "Case#" ++ show n ++ ": " ++ str

group :: [String] -> [[String]]
group [] = []
group xs =
  let
    rowsAndCols = readInts (head xs)
    rows = rowsAndCols !! 0 + 1
  in
    take rows xs : group (drop rows xs)

transform :: [[String]] -> [TestCase]
transform =
  let
    createTestCase gr = case gr of
      (meta:test) -> TestCase (readInts meta) test
  in map createTestCase

process :: TestCase -> String
process testCase =
  case testCase of
    TestCase meta test ->
        show meta ++ show test

readInts :: String -> [Int]
readInts str = map read $ words str
