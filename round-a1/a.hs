data TestCase = TestCase [Int] [String]

main :: IO()
main = do
  contents <- getContents
  let lns = tail . lines $ contents
      testCasses = transform $ group 2 lns
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
  let
    pieces = readInts (test !! 0)
  in
    show $ minEaten1 pieces

minEaten1 :: [Int] -> Int
minEaten1 [x] = 0
minEaten1 (x:y:xs) = (max 0 y - x) + minEaten1 (y:xs)


readInts :: String -> [Int]
readInts str = map read $ words str
