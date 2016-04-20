import System.Environment (getArgs)

main = do
  args <- getArgs
  case args of
    [file1, file2] -> putStrLn "done"
    _ -> putStrLn "error: exactly two arguments needed"
