module Day00Spec (spec) where

import           Test.Hspec
import           Text.Megaparsec (parse)
import qualified Data.Map.Strict as M
import           Data.List (sort)
import           Day00

sample1 :: String
sample1 = unlines [
  "x00: 1",
  "x01: 1",
  "x02: 1",
  "y00: 0",
  "y01: 1",
  "y02: 0",
  "",
  "x00 AND y00 -> z00",
  "x01 XOR y01 -> z01",
  "x02 OR y02 -> z02"
  ]

spec :: Spec
spec = describe "Day00.parseInput" $ do
  it "parses sample wires" $ do
    let Right input = parse parseInput "sample1" sample1
    length (initialWires input) `shouldBe` 6
  it "parses sample gates" $ do
    let Right input = parse parseInput "sample1" sample1
    M.size (gates input) `shouldBe` 3
  it "parses z output gates" $ do
    let Right input = parse parseInput "sample1" sample1
    let zs = M.filter ((== 'z') . head . output) (gates input)
    M.size zs `shouldBe` 3
  describe "Day00.evaluate" $ do
    it "evaluates z00 to 0" $ do
      let Right input = parse parseInput "sample1" sample1
      evaluate input "z00" `shouldBe` 0
    it "evaluates z01 to 0" $ do
      let Right input = parse parseInput "sample1" sample1
      evaluate input "z01" `shouldBe` 0
    it "evaluates z02 to 1" $ do
      let Right input = parse parseInput "sample1" sample1
      evaluate input "z02" `shouldBe` 1
  describe "Day00.bits/int conversions" $ do
    it "bitsToInt handles examples" $ do
      bitsToInt [1,0,0] `shouldBe` 4
      bitsToInt [0] `shouldBe` 0
      bitsToInt [1,1,0,1] `shouldBe` 13
    it "intToBits handles examples" $ do
      intToBits 4 `shouldBe` [1,0,0]
      intToBits 0 `shouldBe` [0]
      intToBits 13 `shouldBe` [1,1,0,1]
    it "roundtrip intToBits/bitsToInt" $ do
      let samples = [0,1,2,3,4,7,8,15,16,31,32,2024]
      map (bitsToInt . intToBits) samples `shouldBe` samples
