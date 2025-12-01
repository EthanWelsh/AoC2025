module Day00
  ( solve
  , parseInput
  , Input(..)
  , Gate(..)
  , GateType(..)
  , evaluate
  , part1
  , bitsToInt
  , intToBits
  ) where

import           Data.Map.Strict      (Map)
import qualified Data.Map.Strict      as M
import           Data.Bits            ((.&.), (.|.), xor, shiftL, shiftR)
import           Data.Function.Memoize (memoize)
import           Data.List            (sort, intercalate)
import qualified Data.Set             as S
import           Text.Megaparsec
import           Utils.Parsers        (Parser)
import           Text.Megaparsec.Char (char, space1, string, lowerChar, digitChar, newline)

-- A wire name (e.g. x00, y04, z12, ntg)
type Wire = String

data GateType = AND | OR | XOR deriving (Show, Eq)
data Gate = Gate { gateType :: GateType
                 , left     :: Wire
                 , right    :: Wire
                 , output   :: Wire
                 } deriving (Show, Eq)

data Input = Input { initialWires :: Map Wire Int
                   , gates        :: Map Wire Gate
                   } deriving (Show)

-- Parse a wire name: one or more lowercase letters or digits.
wireP :: Parser Wire
wireP = some (lowerChar <|> digitChar)

bitP :: Parser Int
bitP = (
    (0 <$ char '0') <|>
    (1 <$ char '1')
  )

assignmentP :: Parser (Wire, Int)
assignmentP = do
  w <- wireP
  _ <- string ": "
  b <- bitP
  return (w, b)

gateTypeP :: Parser GateType
gateTypeP =  (AND <$ string "AND")
         <|> (OR  <$ string "OR")
         <|> (XOR <$ string "XOR")

gateLineP :: Parser Gate
gateLineP = do
  w1 <- wireP
  space1
  gt <- gateTypeP
  space1
  w2 <- wireP
  space1
  _ <- string "->"
  space1
  out <- wireP
  return (Gate gt w1 w2 out)

-- Parse the initial assignments section (terminated by a blank line)
assignmentsSectionP :: Parser [(Wire, Int)]
assignmentsSectionP = manyTill (assignmentP <* newline) newline

gatesSectionP :: Parser [Gate]
gatesSectionP = gateLineP `sepEndBy` newline

parseInput :: Parser Input
parseInput = do
  assigns <- assignmentsSectionP
  gs <- gatesSectionP
  eof
  let gateMap = M.fromList [(output g, g) | g <- gs]
  return (Input (M.fromList assigns) gateMap)

evaluateHelper :: Input -> Wire -> Int
evaluateHelper input wire = case M.lookup wire (initialWires input) of
  Just val -> val
  Nothing -> case M.lookup wire (gates input) of
    Just gate -> let lval = evaluate input (left gate)
                     rval = evaluate input (right gate)
                     result = case gateType gate of
                       AND -> lval .&. rval
                       OR  -> lval .|. rval
                       XOR -> lval `xor` rval
                 in result
    Nothing -> error ("Wire not found: " ++ wire)

evaluate :: Input -> Wire -> Int
evaluate input = memoize (evaluateHelper input)

allWiresMatching :: Input -> (Wire -> Bool) -> [Wire]
allWiresMatching input predicate = let
  a = M.keys (gates input)
  b = M.keys (initialWires input)
  allWires = a ++ b
  in sort $ filter predicate allWires

allOutputGates :: Input -> [Wire]
allOutputGates input = allWiresMatching input (('z' ==) . head)

-- Convert MSB-first list of bits to a non-negative integer.
bitsToInt :: [Int] -> Int
bitsToInt = foldl (\acc x -> (acc `shiftL` 1) .|. x) 0

-- Convert a non-negative integer into its binary representation as a list
-- of bits (MSB-first). For example, intToBits 4 == [1,0,0].
intToBits :: Int -> [Int]
intToBits n
  | n < 0     = error "fromOutput: negative input"
  | n == 0    = [0]
  | otherwise = reverse (go n)
  where
    go 0 = []
    go k = let bit = if (k .&. 1) == 1 then 1 else 0
            in bit : go (k `shiftR` 1)

part1 :: Input -> Int
part1 input = let 
  gs = allOutputGates input
  vs = reverse $ map (evaluate input) gs
  in bitsToInt vs

-- Find a gate that takes two specific inputs (order-independent) and has a specific type
findGate :: Input -> Wire -> Wire -> GateType -> Maybe Gate
findGate input w1 w2 gt = 
  let allGates = M.elems (gates input)
      matches g = gateType g == gt && 
                  ((left g == w1 && right g == w2) || (left g == w2 && right g == w1))
  in case filter matches allGates of
       [g] -> Just g
       _   -> Nothing

-- Get the gate that produces a specific wire
getGate :: Input -> Wire -> Maybe Gate
getGate input wire = M.lookup wire (gates input)

-- Find swapped wires by checking adder structure bit by bit
findSwappedWires :: Input -> [Wire]
findSwappedWires input = 
  let allGates = M.elems (gates input)
      
      -- Rule 1: If output starts with 'z' and it's not the last bit, it must be XOR
      maxZ = maximum [read (drop 1 w) :: Int | w <- M.keys (gates input), head w == 'z']
      finalZ = "z" ++ (if maxZ < 10 then "0" else "") ++ show maxZ
      
      wrongZ = [output g | g <- allGates,
                head (output g) == 'z',
                output g /= "z00",
                output g /= finalZ,
                gateType g /= XOR]
      
      -- Rule 2: If XOR gate doesn't have x,y inputs and doesn't output to z, it's wrong
      wrongXOR = [output g | g <- allGates,
                  gateType g == XOR,
                  head (left g) `notElem` ['x', 'y'],
                  head (right g) `notElem` ['x', 'y'],
                  head (output g) /= 'z']
      
      -- Rule 3: If AND gate has x,y inputs (except x00,y00) and feeds into XOR, it's wrong
      wrongAND = [output g | g <- allGates,
                  gateType g == AND,
                  left g /= "x00" && right g /= "x00",
                  let out = output g,
                  any (\g2 -> (left g2 == out || right g2 == out) && gateType g2 == XOR) allGates]
      
      -- Rule 4: If XOR gate has x,y inputs (except x00,y00) and doesn't feed into XOR, it's wrong
      wrongInputXOR = [output g | g <- allGates,
                       gateType g == XOR,
                       ((head (left g) == 'x' && head (right g) == 'y') ||
                        (head (left g) == 'y' && head (right g) == 'x')),
                       left g /= "x00" && right g /= "x00",
                       let out = output g,
                       not (any (\g2 -> (left g2 == out || right g2 == out) && gateType g2 == XOR) allGates)]
      
  in S.toList $ S.fromList (wrongZ ++ wrongXOR ++ wrongAND ++ wrongInputXOR)

part2 :: Input -> String
part2 input = 
  let swapped = findSwappedWires input
  in intercalate "," (sort swapped)


solve :: FilePath -> IO ()
solve filePath = do
  contents <- readFile filePath
  case parse parseInput filePath contents of
          Left eb -> putStr (errorBundlePretty eb)
          Right input -> do
            putStrLn $ "Part 1: " ++ show (part1 input)
            putStrLn $ "Part 2: " ++ part2 input
