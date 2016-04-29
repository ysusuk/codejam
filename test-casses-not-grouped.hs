data TestCase = TestCase String

main :: IO()
main = do
  contents <- getContents
  let lns = tail . lines $ contents
      testCasses = transform lns
      outs = map process testCasses
  putStr $ unlines (map appendCaseAndNum $ zip [1..] outs)

appendCaseAndNum :: (Int, String) -> String
appendCaseAndNum (n, str) = "Case#" ++ show n ++ ": " ++ str

transform :: [String] -> [TestCase]
transform = map TestCase

process :: TestCase -> String
process (TestCase test) =
  show test

readInts :: String -> [Int]
readInts str = map read $ words str
