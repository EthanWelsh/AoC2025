module Utils.Graph (
  Graph,
  edges,
  graphFromEdges,
  graphAsMap,
  graphFromMap,
  apply,
  addEdge,
  removeEdge,
  removeBidirectionalEdge,
  neighbors,
  nodes,
  reachable,
  connectedComponents,
  makeBidirectional
) where

import           Data.List       ((\\))
import           Data.List.Extra (groupSort)
import           Data.Map        as M (Map, adjust, insertWith, keys,
                                       fromList, toList, (!))
import           Data.Set        (Set, empty, insert, member,
                                  notMember, unions)
import Data.Tuple (swap)

newtype Graph a = Graph (Map a [a]) deriving (Show)

makeBidirectional :: Ord a => Graph a -> Graph a
makeBidirectional g = let
  allEdges = edges g ++ map swap (edges g)
  in graphFromEdges allEdges

-- | Return all edges in the graph as pairs (src, dst).
edges :: Graph a -> [(a, a)]
edges g = concatMap flatten (M.toList (graphAsMap g))

flatten :: (a, [b]) -> [(a, b)]
flatten (a, bs) = map (a,) bs

unflatten :: Ord a => [(a, b)] -> [(a, [b])]
unflatten = groupSort

-- | Build a 'Graph' from a list of directed edges.
graphFromEdges :: Ord a => [(a, a)] -> Graph a
graphFromEdges es = graphFromMap $ M.fromList $ unflatten es

-- | Apply a transformation to the underlying map representation.
apply :: (Map a [a] -> Map a [a]) -> Graph a -> Graph a
apply f (Graph g) = Graph $ f g

-- | Expose the underlying map representation of the graph.
graphAsMap :: Graph a -> Map a [a]
graphAsMap (Graph m) = m

-- | Construct a 'Graph' directly from a map of adjacency lists.
graphFromMap :: Map a [a] -> Graph a
graphFromMap = Graph

-- | Add a directed edge (src -> dst) to the graph.
addEdge :: Ord a => Graph a -> a -> a -> Graph a
addEdge g src dst = apply (M.insertWith (++) src [dst]) g

-- | Remove a directed edge (src -> dst) from the graph.
removeEdge :: Ord a => Graph a -> (a, a) -> Graph a
removeEdge g (src, dst) = apply (adjust (\es -> es \\ [dst]) src) g

-- | Remove an edge and its reverse (both directions) from the graph.
removeBidirectionalEdge :: Ord a => Graph a -> (a, a) -> Graph a
removeBidirectionalEdge g e = let
  g1 = removeEdge g e
  g2 = removeEdge g1 (swap e)
  in g2

-- | Return the adjacency list for a given node.
neighbors :: Ord a => Graph a -> a -> [a]
neighbors (Graph g) n = g ! n

-- | List all nodes present in the graph.
nodes :: Graph a -> [a]
nodes (Graph g) = keys g

-- | Compute the set of nodes reachable from a start node (including it).
reachable :: Ord a => Graph a -> a -> Set a
reachable g = helper g empty
  where
    helper gg visited nn
      | nn `member` visited = visited
      | otherwise = let
        newVisited = insert nn visited
        ns = neighbors gg nn
        in unions $ map (helper gg newVisited) ns

-- | Partition the graph into connected components (as sets of nodes).
connectedComponents :: Ord a => Graph a -> [Set a]
connectedComponents g = connectedHelper g (nodes g)
  where
    connectedHelper _ [] = []
    connectedHelper gg (n:ns) = let
      reachableNodes = reachable gg n
      unvisited = filter (`notMember` reachableNodes) ns
      in reachableNodes:connectedHelper gg unvisited
