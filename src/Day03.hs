module Day03 (solve, parseInput) where

import Text.Megaparsec (parse, errorBundlePretty, some, optional, sepBy)
import Text.Megaparsec.Char (digitChar, eol)
import Utils.Parsers (Parser)

type Bank = [Int]
type Input = [Bank]

charsToInts :: [Char] -> [Int]
charsToInts cs = map (\c -> read [c]) cs

parseInput :: Parser Input
parseInput = do
  lines <- (some digitChar) `sepBy` eol
  return $ map charsToInts lines

-- Remove the last element in the list.
dropLast :: [a] -> [a]
dropLast [x] = []
dropLast (x:xs) = x : (dropLast xs)

highestVoltage :: Bank -> Int
highestVoltage bank = let
  a = maximum (dropLast bank)
  b = maximum $ tail $ dropWhile (/= a) bank
  in (a * 10) + b

part1 :: Input -> IO ()
part1 input = do
  putStr "Part 1: "
  let voltages = map highestVoltage input
  print $ sum voltages

part2 :: Input -> IO ()
part2 input = do
  putStr "Part 2: "
  --print $ sum (map length input)

solve :: FilePath -> IO ()
solve filePath = do
  contents <- readFile filePath
  case parse parseInput filePath contents of
          Left eb -> putStr (errorBundlePretty eb)
          Right input -> do
            part1 input
            part2 input
