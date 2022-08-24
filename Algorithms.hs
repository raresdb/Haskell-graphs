module Algorithms where

import qualified Data.Set as S
import StandardGraph

type Graph a = StandardGraph a

{-
    Generic search function for a graph.
-}
aux_search  :: Ord a
            => ([a] -> [a] -> [a])  -- Function for combining the lists of nodes.
            -> a                    -- start node
            -> S.Set a            -- already visited nodes
            -> [a]                  -- waiting list
            -> Graph a              -- graph
            -> [a]                  -- final list of all visited nodes in order
aux_search f node visited next graph
    | null fullNext = [node]
    | S.member (head fullNext) visited = aux_search f node visited (tail fullNext) graph
    | otherwise = node : aux_search f (head fullNext)
    (S.insert (head fullNext) visited) (tail fullNext) graph
    where fullNext = f next [n | n <- S.toList (outNeighbors node graph), S.notMember n visited]

search :: Ord a
       => ([a] -> [a] -> [a])  -- Function for combining lists of nodes.
       -> a                    -- start node
       -> Graph a              -- graph
       -> [a]                  -- final list
search f node graph = aux_search f node (S.fromList [node]) [] graph

{-
    Using the previous function to compute BFS.

    Example:

    > bfs 1 graph4
    [1,2,3,4]

    > bfs 4 graph4
    [4,1,2,3]
-}
bfs :: Ord a => a -> Graph a -> [a]
bfs = search (++)

{-
    Same with DFS.

    Example:

    > dfs 1 graph4 
    [1,2,4,3]
    
    > dfs 4 graph4
    [4,1,2,3]
-}
dfs :: Ord a => a -> Graph a -> [a]
dfs = search (\l1 l2 -> l2 ++ l1)

{-
    Counts the number of nodes that each search(BFS, DFS) found in between a
    source and destination.

    Example:

    > countIntermediate 1 3 graph4
    Just (1,2)

    > countIntermediate 3 1 graph4
    Nothing
-}
countIntermediate :: Ord a
                  => a                 -- source node
                  -> a                 -- dest node
                  -> StandardGraph a   -- graph
                  -> Maybe (Int, Int)  -- number of nodes
countIntermediate from to graph
    | notElem to pathDFS = Nothing
    | otherwise = Just (length (fst (span (/= to) pathBFS)) - 1, length (fst (span (/= to) pathDFS)) - 1)
    where
        pathDFS = dfs from graph
        pathBFS = bfs from graph
        