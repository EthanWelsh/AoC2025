module Day04 (solve) where

import Text.Megaparsec
import Text.Megaparsec.Char (char, eol)
import Utils.Maze
import Utils.Parsers (Parser)

type Grid = Maze Char

type Input = Grid

parseInput :: Parser Input
parseInput = do
  ls <- many (char '.' <|> char '@') `sepBy` eol
  return $ mazeFromList ls

countOpenSpaces :: Grid -> Point -> Int
countOpenSpaces g p = length $ filter (\pp -> getPoint g pp == '@') (neighbors8 g p)

part1 :: Input -> IO ()
part1 input = do
  putStr "Part 1: "
  let accessibleSpots = allPointsSatisfying input (\p -> (getPoint input p == '@') && countOpenSpaces input p < 4)
  print $ length accessibleSpots

clear :: Grid -> Grid
clear g =
  let accessibleSpots = allPointsSatisfying g (\p -> (getPoint g p == '@') && countOpenSpaces g p < 4)
   in setPoints g accessibleSpots '.'

repeatUntilStable :: (Eq a) => (a -> a) -> a -> a
repeatUntilStable f x =
  let x' = f x
   in if x == x' then x else repeatUntilStable f x'

part2 :: Input -> IO ()
part2 input = do
  let countBefore = length $ findPoints input (== '@')
  let finalGrid = repeatUntilStable clear input
  let countAfter = length $ findPoints finalGrid (== '@')
  putStr "Part 2: "
  print $ countBefore - countAfter

solve :: FilePath -> IO ()
solve filePath = do
  contents <- readFile filePath
  case parse parseInput filePath contents of
    Left eb -> putStr (errorBundlePretty eb)
    Right input -> do
      part1 input
      part2 input
