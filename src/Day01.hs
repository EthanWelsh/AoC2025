module Day01 (solve) where

type Input = String

parseInput :: String -> Input
parseInput = id

part1 :: Input -> String
part1 input = "Not implemented"

part2 :: Input -> String
part2 input = "Not implemented"

solve :: FilePath -> IO ()
solve filePath = do
  contents <- readFile filePath
  let input = parseInput contents
  putStrLn $ "Part 1: " ++ part1 input
  putStrLn $ "Part 2: " ++ part2 input
