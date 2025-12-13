{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}
module Day10 (solve) where

import Text.Megaparsec (parse, errorBundlePretty, between, sepBy, many, (<|>))
import Utils.Parsers (Parser, sc, lexeme, symbol, integer)
import Text.Megaparsec.Char (char)
import Algorithm.Search
import Control.Lens (element, (%~), (&))


data Light = On | Off deriving (Show, Eq, Ord)
newtype WiringSchematic = WiringSchematic [Int] deriving (Show, Eq, Ord)
newtype Joltage = Joltage [Int] deriving (Show, Eq, Ord)

data Machine = Machine
  { targetLights :: [Light]
  , schematics :: [WiringSchematic]
  , requirements :: Joltage
  } deriving (Show, Eq, Ord)

type Input = [Machine]

-- The manual describes one machine per line. 
-- Each line contains:
--   - a single indicator light diagram in [square brackets]
--   - one or more button wiring schematics in (parentheses)
--   - joltage requirements in {curly braces}.
-- Example lines:
-- [.##.] (3) (1,3) (2) (2,3) (0,2) (0,1) {3,5,4,7}
-- [...#.] (0,2,3,4) (2,3) (0,4) (0,1,2) (1,2,3,4) {7,5,12,7,2}
-- [.###.#] (0,1,2,3,4) (0,3,4) (0,1,2,4,5) (1,2) {10,11,11,5,10,5}
parseLights :: Parser [Light]
parseLights = do
  lightDiagram <- between (symbol "[") (symbol "]") (lexeme (many (char '#' <|> char '.')))
  return $ map (\c -> if c == '#' then On else Off) lightDiagram

parseSchematics :: Parser [WiringSchematic]
parseSchematics = do
  xs <- many (between (symbol "(") (symbol ")") (sepBy integer (symbol ",")))
  return $ map WiringSchematic xs

parseRequirements :: Parser Joltage
parseRequirements = do
  joltageReqs <- between (symbol "{") (symbol "}") (sepBy integer (symbol ","))
  return $ Joltage joltageReqs

machineParser :: Parser Machine
machineParser = Machine <$> parseLights <*> parseSchematics <*> parseRequirements

parseInput :: Parser Input
parseInput = sc >> many machineParser

toggleLights :: [Light] -> WiringSchematic -> [Light]
toggleLights ls (WiringSchematic indices) = foldl toggle ls indices
  where
    toggle :: [Light] -> Int -> [Light]
    toggle xs i = xs & element i %~ (\l -> if l == On then Off else On)

solveLights :: Machine -> Int
solveLights m = case shortestPath of
    Just (c, _) -> c
    Nothing -> error "No solution found"
  where
    shortestPath :: Maybe (Int, [[Light]])
    shortestPath = dijkstra neighbors cost isGoal initialState

    neighbors :: [Light] -> [[Light]]
    neighbors lights = map (toggleLights lights) (schematics m)

    cost :: [Light] -> [Light] -> Int
    cost _ _ = 1

    isGoal :: [Light] -> Bool
    isGoal lights = lights == targetLights m

    initialState :: [Light]
    initialState = replicate (length (targetLights m)) Off

part1 :: Input -> IO ()
part1 input = do
  putStr "Part 1: "
  print $ sum $ map solveLights input

increaseJoltage :: Joltage -> WiringSchematic -> Joltage
increaseJoltage (Joltage ls) (WiringSchematic indices) = Joltage $ foldl increase ls indices
  where
    increase :: [Int] -> Int -> [Int]
    increase xs i = xs & element i %~ (+ 1)

higherThanTarget :: Joltage -> Joltage -> Bool
higherThanTarget (Joltage current) (Joltage target) = any (uncurry (>)) (zip current target)

solveJoltage :: Machine -> Int
solveJoltage m = case shortestPath of
    Just (c, _) -> c
    Nothing -> error "No solution found"
  where
    shortestPath :: Maybe (Int, [Joltage])
    shortestPath = aStar (neighbors `pruning` check) cost estimatedCostToGoal isGoal initialState

    check :: Joltage -> Bool
    check j = higherThanTarget j (requirements m)

    neighbors :: Joltage -> [Joltage]
    neighbors joltage = map (increaseJoltage joltage) (schematics m)

    cost :: Joltage -> Joltage -> Int
    cost _ _ = 1

    estimatedCostToGoal :: Joltage -> Int
    estimatedCostToGoal (Joltage current) = sum $ zipWith diff current target
      where
        (Joltage target) = requirements m
        diff a b = max 0 (b - a)

    isGoal :: Joltage -> Bool
    isGoal joltage = joltage == requirements m

    initialState :: Joltage
    initialState = Joltage $ replicate (length (targetLights m)) 0

part2 :: Input -> IO ()
part2 input = do
  putStr "Part 2: "
  print $ sum $ map solveJoltage input

solve :: FilePath -> IO ()
solve filePath = do
  contents <- readFile filePath
  case parse parseInput filePath contents of
          Left eb -> putStr (errorBundlePretty eb)
          Right input -> do
            part1 input
            part2 input
