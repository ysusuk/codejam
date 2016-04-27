data TestCase = TestCase Int Int [String]

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
    rows = rowsAndCols !! 0
  in
    take rows xs : (group (drop rows xs))

transform :: [[String]] -> [TestCase]
transform =
  let
    createTestCase gr = case gr of
      (str:strs) ->
        let
          rowsAndCols = readInts str
        in
          TestCase (rowsAndCols !! 0) (rowsAndCols !! 1) strs
  in map createTestCase

process :: TestCase -> String
process testCase =
  case testCase of
    TestCase r c str ->
      let
        rows = r
        cols = c
        grid = str
      in
        show r ++ show c ++ show grid

readInts :: String -> [Int]
readInts str = map read $ words str
