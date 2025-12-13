{-# OPTIONS_GHC -Wno-unused-top-binds #-}
module Day08 (solve) where

import Text.Megaparsec
import Utils.Parsers (Parser)
import Text.Megaparsec.Char (char, eol)
import Utils.Graph (Graph, graphFromNodes, connectedComponents, addBidirectionalEdge, addBidirectionalEdges)
import qualified Text.Megaparsec.Char.Lexer as L
import Data.List (sortBy, tails)
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

closestPairs :: [Point3D] -> [(Point3D, Point3D)]
closestPairs pts = let
  ps = allPairs pts
  sortedPs = sortBy (\(a,b) (c,d) -> compare (distanceSquared a b) (distanceSquared c d)) ps
  in sortedPs

part1 :: Input -> IO ()
part1 input = do
  putStr "Part 1: "
  let toMerge = take 1000 (closestPairs input)
  let graph = graphFromPoints input
  let mergedGraph = addBidirectionalEdges graph toMerge
  let components = connectedComponents mergedGraph
  print $ product $ take 3 $ rsort $ map length components

-- | Merged edges until all points are connected. Returns the last point to be merged, causing the graph to be fully connected.
mergeTillConnected :: Graph Point3D -> [(Point3D, Point3D)] -> (Point3D, Point3D)
mergeTillConnected _ [] = error "No pairs to merge"
mergeTillConnected g (x:xs) = let 
  g' = addBidirectionalEdge g x
  components = connectedComponents g'
  in if length components == 1 then x else mergeTillConnected g' xs

part2 :: Input -> IO ()
part2 input = do
  putStr "Part 2: "
  let pairs = closestPairs input
  let graph = graphFromPoints input
  let ((x1, _, _), (x2, _, _)) = mergeTillConnected graph pairs
  print $ x1 * x2

solve :: FilePath -> IO ()
solve filePath = do
  contents <- readFile filePath
  case parse parseInput filePath contents of
          Left eb -> putStr (errorBundlePretty eb)
          Right input -> do
            part1 input
            part2 input
