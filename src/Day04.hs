module Day04 (solve) where

import Text.Megaparsec
import           Utils.Parsers (Parser)
--import Control.Monad (void)
import Text.Megaparsec.Char (string, char)
import Text.Megaparsec.Char (eol)
import Utils.Maze

type Grid = Maze Char
type Input = Grid

parseInput :: Parser Input
parseInput = do 
  ls <- many (char '.' <|> char '@') `sepBy` eol
  return $ mazeFromList ls

countOpenSpaces :: Grid -> Point -> Int
countOpenSpaces g p = length $ filter (\pp -> (getPoint g pp) =='@') (neighbors8 g p)

part1 :: Input -> IO ()
part1 input = do
  putStr "Part 1: "
  let accessibleSpots = allPointsSatisfying input (\p -> (getPoint input p == '@') && (countOpenSpaces input p) < 4)
  print $ length accessibleSpots

part2 :: Input -> IO ()
part2 input = do
  putStr "Part 2: "

solve :: FilePath -> IO ()
solve filePath = do
  contents <- readFile filePath
  case parse parseInput filePath contents of
          Left eb -> putStr (errorBundlePretty eb)
          Right input -> do
            part1 input
            part2 input

