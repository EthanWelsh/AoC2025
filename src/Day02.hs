module Day02 (solve) where

import Text.Megaparsec
import           Utils.Parsers (Parser, integer)
import Text.Megaparsec.Char (char)
import Data.List.Split (chunksOf)

type Input = [Range]
type Range = (Int, Int)

parseInput :: Parser Input
parseInput = parseRange `sepBy` (char ','  )

parseRange :: Parser Range
parseRange = do
  open <- integer
  _ <- char '-'
  close <- integer
  return (open, close)

numsInRange :: Range -> [Int]
numsInRange (start, close) = [start..close]

splitInHalf :: [a] -> ([a], [a])
splitInHalf xs = splitAt (length xs `div` 2) xs

isInvalid :: Int -> Bool
isInvalid x = let
  n = show x
  (a, b) = splitInHalf n
  in a == b

allNums :: [Range] -> [Int]
allNums rs = concatMap numsInRange rs

part1 :: Input -> IO ()
part1 input = do
  putStr "Part 1: "
  let invalidNums = filter isInvalid (allNums input)
  print $ sum invalidNums

-- [1234] --> [["12", "34"], ["1", "2", "3", "4"]]
allEqualSizedChunks :: [a] -> [[[a]]]
allEqualSizedChunks xs = let
  n = length xs
  possibleChunkSizes = filter (\x -> n `mod` x == 0) [1 .. n `div` 2]
  in map (\c -> chunksOf c xs) possibleChunkSizes

chunkInvalid :: Eq a => [[a]] -> Bool
chunkInvalid chunks = all (== head chunks) (tail chunks)

isInvalid2 :: Int -> Bool
isInvalid2 x = let
  n = show x
  possibleChunkSizes = allEqualSizedChunks n
  in any chunkInvalid possibleChunkSizes

part2 :: Input -> IO ()
part2 input = do
  putStr "Part 2: "
  let invalidNums = filter isInvalid2 (allNums input)
  print $ sum invalidNums

solve :: FilePath -> IO ()
solve filePath = do
  contents <- readFile filePath
  case parse parseInput filePath contents of
          Left eb -> putStr (errorBundlePretty eb)
          Right input -> do
            part1 input
            part2 input
