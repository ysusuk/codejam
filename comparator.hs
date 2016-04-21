import System.Environment (getArgs)

main = do
  args <- getArgs
  case args of
    [file1, file2] -> do
      in1 <- readFile file1
      in2 <- readFile file2
      let
        result x y = (if x /= y then "\n/=" else "") ++ "\n" ++ x ++ "\n" ++ y
        out = zipWith result (lines in1) (lines in2)
      putStrLn $ unlines out

    _ -> putStrLn "error: exactly two arguments needed"
