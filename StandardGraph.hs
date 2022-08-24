{-# LANGUAGE TupleSections #-}
module StandardGraph where

import qualified Data.Set as S

{-
    Standard graph represented as a data type with nodes and edges as fields.
-}
data StandardGraph a = SG
    {
        nodes :: S.Set a,
        edges :: S.Set (a, a)
    }
    deriving (Eq, Show)

{-
    Graph from nodes and arcs.
-}
fromComponents :: Ord a
               => [a]              -- nodes
               -> [(a, a)]         -- arcs
               -> StandardGraph a  -- graph
fromComponents ns es = SG (S.fromList ns) (S.fromList es)

{-
    Graph examples.
-}
graph1 :: StandardGraph Int
graph1 = fromComponents [1, 2, 3, 3, 4] [(1, 2), (1, 3), (1, 2)]

graph2 :: StandardGraph Int
graph2 = fromComponents [4, 3, 3, 2, 1] [(1, 3), (1, 3), (1, 2)]

graph3 :: StandardGraph Int
graph3 = fromComponents [1, 2, 3, 4] [(1, 2), (1, 4), (4, 1), (2, 3), (1, 3)]

graph4 :: StandardGraph Int
graph4 = fromComponents [1, 2, 3, 4] [(1, 2), (1, 4), (4, 1), (2, 4), (1, 3)]

shouldBeTrue :: Bool
shouldBeTrue = graph1 == graph2

{-
    Nodes to which arcs get from the current node.
    Example:

    > outNeighbors 1 graph3
    fromList [2,3,4]
-}
outNeighbors :: Ord a => a -> StandardGraph a -> S.Set a
outNeighbors node graph = S.fromList [snd e | e <- S.toList (edges graph), fst e == node]

{-
    Nodes from which arcs go to the current node.
    Example:

    > inNeighbors 1 graph3 
    fromList [4]
-}
inNeighbors :: Ord a => a -> StandardGraph a -> S.Set a
inNeighbors node graph = S.fromList [fst e | e <- S.toList (edges graph), snd e == node]

{-
    Remove node from graph.

    Example:

    > removeNode 1 graph3
    (fromList [2,3,4],fromList [(2,3)])
-}
removeNode :: Ord a => a -> StandardGraph a -> StandardGraph a
removeNode node graph = fromComponents (S.toList (S.delete node (nodes graph)))
    [e | e <- S.toList (edges graph), fst e /= node, snd e /= node]

{-
    Splits a node into a list of nodes by keeping the arcs.

    Example:

    > splitNode 2 [5,6] graph3
    (fromList [1,3,4,5,6],fromList [(1,3),(1,4),(1,5),(1,6),(4,1),(5,3),(6,3)])
-}
splitNode :: Ord a
          => a                -- target node
          -> [a]              -- replacements
          -> StandardGraph a  -- current graph
          -> StandardGraph a  -- new graph
splitNode old news graph = fromComponents
    (S.toList (S.union (S.delete old (nodes graph)) (S.fromList news)))
    (foldl (++) [] (map (\n -> [(neigh, n) | neigh <- S.toList (inNeighbors old graph)]
    ++ [(n, neigh) | neigh <- S.toList (outNeighbors old graph)]) news)
    ++ S.toList (edges (removeNode old graph)))

{-
    Merge nodes that respect a given condition.

    Example:

    > mergeNodes even 5 graph3
    (fromList [1,3,5],fromList [(1,3),(1,5),(5,1),(5,3)])
-}
mergeNodes :: Ord a
           => (a -> Bool)      -- condition to respect
           -> a                -- new node
           -> StandardGraph a  -- current graph
           -> StandardGraph a  -- new graph
mergeNodes prop node graph = foldl (\acc n -> splitNode n [node] acc) graph
    [n | n <- S.toList (nodes graph), prop n] 