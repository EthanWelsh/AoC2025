module Main (main) where

import qualified Day00
import qualified Day01
import qualified Day02
import qualified Day03
import qualified Day04
import qualified Day05
import qualified Day06
import qualified Day07
import qualified Day08
import qualified Day09
import qualified Day10
import qualified Day11
import qualified Day12

import           System.Environment (getArgs)

solvers :: [FilePath -> IO ()]
solvers =
    [ Day00.solve
    , Day01.solve
    , Day02.solve
    , Day03.solve
    , Day04.solve
    , Day05.solve
    , Day06.solve
    , Day07.solve
    , Day08.solve
    , Day09.solve
    , Day10.solve
    , Day11.solve
    , Day12.solve
    ]

main :: IO ()
main = do
    [day, filePath] <- getArgs
    let solver = solvers !! read day
    putStrLn ""
    solver filePath
