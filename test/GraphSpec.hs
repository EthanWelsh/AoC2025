module GraphSpec (spec) where

import Test.Hspec
import Utils.Graph
import Data.Map as M
import Data.Set as S

spec :: Spec
spec = do
  describe "Graph" $ do
    it "adds edges correctly" $ do
      let g = graphFromNodes [1, 2, 3]
      let edgesToAdd = [(1, 2), (2, 3)]
      let g' = addEdges g edgesToAdd
      let m = graphAsMap g'
      M.lookup 1 m `shouldBe` Just [2]
      M.lookup 2 m `shouldBe` Just [3]
      M.lookup 3 m `shouldBe` Just []

    it "handles bidirectional edges" $ do
      let g = graphFromNodes [1, 2]
      let g' = addEdges g [(1, 2)]
      let gBi = makeBidirectional g'
      let m = graphAsMap gBi
      M.lookup 1 m `shouldBe` Just [2]
      M.lookup 2 m `shouldBe` Just [1]

    it "calculates reachable nodes correctly" $ do
      let g = graphFromNodes [1, 2, 3]
      let g' = addEdges g [(1, 2)]
      reachable g' 1 `shouldBe` S.fromList [1, 2]
      reachable g' 2 `shouldBe` S.fromList [2]
      reachable g' 3 `shouldBe` S.fromList [3]

