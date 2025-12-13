{-# OPTIONS_GHC -Wno-unused-top-binds #-}
module Day08 (solve) where

import Text.Megaparsec
import Utils.Parsers (Parser)
import Text.Megaparsec.Char (char, eol)
import Utils.Graph (Graph, graphFromNodes, connectedComponents, addEdges)
import qualified Text.Megaparsec.Char.Lexer as L
import Data.List (sort, sortBy, tails)
import Data.Ord (comparing, Down (Down))

type Point3D = (Int, Int, Int)
type Input = [Point3D]

parsePosition :: Parser Point3D
parsePosition = (,,) <$> L.decimal <* char ',' <*> L.decimal <* char ',' <*> L.decimal

parseInput :: Parser Input
parseInput = parsePosition `sepEndBy` eol

graphFromPoints :: [Point3D] -> Graph Point3D
graphFromPoints = graphFromNodes

euclideanDistance :: Point3D -> Point3D -> Double
euclideanDistance (x1, y1, z1) (x2, y2, z2) =
    sqrt $ fromIntegral $ (x1 - x2)^(2::Int) + (y1 - y2)^(2::Int) + (z1 - z2)^(2::Int)

distanceSquared :: Point3D -> Point3D -> Int
distanceSquared = xdistanceSquared
  where
    xdistanceSquared (x1, y1, z1) (x2, y2, z2) = (x1 - x2)^(2::Int) + (y1 - y2)^(2::Int) + (z1 - z2)^(2::Int)

allPairs :: [a] -> [(a, a)]
allPairs xs = [(x, y) | (x:ys) <- tails xs, y <- ys]

rsort :: Ord a => [a] -> [a]
rsort = sortBy (comparing Down)

part1 :: Input -> IO ()
part1 input = do
  putStr "Part 1: "
  let ps = allPairs input
  let sortedPs = sortBy (\(a,b) (c,d) -> compare (distanceSquared a b) (distanceSquared c d)) ps
  let toMerge = concatMap (\(a, b) -> [(a, b), (b, a)]) $ take 1000 sortedPs
  let graph = graphFromPoints input
  let mergedGraph = addEdges graph toMerge
  let components = connectedComponents mergedGraph
  print $ product $ take 3 $ rsort $ map length components


part2 :: Input -> IO ()
part2 _ = do
  putStr "Part 2: "
  --print input

solve :: FilePath -> IO ()
solve filePath = do
  contents <- readFile filePath
  case parse parseInput filePath contents of
          Left eb -> putStr (errorBundlePretty eb)
          Right input -> do
            part1 input
            part2 input
