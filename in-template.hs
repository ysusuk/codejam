data TestCase = TestCase Int String String

main :: IO()
main = do
  contents <- getContents
  let lns = tail . lines $ contents
      groupedLns = group 3 lns

  putStr ""

appendCase = (++  "Case#")

group :: Int -> [a] -> [[a]]
group _ [] = []
group n list
 | n > 0 = (take n list) : (group n (drop n list))
 | otherwise = error "Negarive n"

transform :: [[a]] -> [TestCase]
transform l = map (\gr -> TestCase (read (head gr) :: Int) (gr !! 1) (gr !! 2)) l

process :: Int -> String -> String -> String
process n str1 str2 =
  let
    ints1 = readInts str1
    ints2 = readInts str2
  in
    show n ++ show ints1 ++ show ints2

readInts :: String -> [Int]
readInts str = map read $ words str
