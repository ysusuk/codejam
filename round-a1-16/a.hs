import Data.List

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
    h = head test : []
    permuts = permutations (tail test)
    permuts1 = map (++ h) permuts
    permuts2 = map (h ++) permuts
  in
    head (reverse (sort (permuts1 ++ permuts2)))

readInts :: String -> [Int]
readInts str = map read $ words str
