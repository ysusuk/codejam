data TestCase = TestCase [Int] [String]

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
    createTestCase (meta:test) = TestCase (readInts meta) test
  in map createTestCase

process :: TestCase -> String
process (TestCase meta test) =
  show meta ++ show test

readInts :: String -> [Int]
readInts str = map read $ words str
