module AlgebraicGraph where

import qualified Data.Set as S

data AlgebraicGraph a
    = Empty
    | Node a
    | Overlay (AlgebraicGraph a) (AlgebraicGraph a)
    | Connect (AlgebraicGraph a) (AlgebraicGraph a)

-- (1, 2), (1, 3)
angle :: AlgebraicGraph Int
angle = Connect (Node 1) (Overlay (Node 2) (Node 3))

-- (1, 2), (1, 3), (2, 3)
triangle :: AlgebraicGraph Int
triangle = Connect (Node 1) (Connect (Node 2) (Node 3))

{-
    Node list.
-}
nodes :: Ord a => AlgebraicGraph a -> S.Set a
nodes Empty = S.empty
nodes (Node node) = S.fromList([node])
nodes (Overlay g1 g2) = S.union (nodes g1) (nodes g2)
nodes (Connect g1 g2) = S.union (nodes g1) (nodes g2)

{-
    Arcs list.
-}
edges :: Ord a => AlgebraicGraph a -> S.Set (a, a)
edges (Overlay g1 g2) = S.union (edges g1) (edges g2)
edges (Connect g1 g2) = S.union (S.cartesianProduct (nodes g1) (nodes g2)) (edges (Overlay g1 g2))
edges graph = S.empty

{-
    List of nodes that the current node points to.
-}
outNeighbors :: Ord a => a -> AlgebraicGraph a -> S.Set a
outNeighbors node (Overlay g1 g2) = S.union (outNeighbors node g1) (outNeighbors node g2)
outNeighbors node (Connect g1 g2) = S.union (outNeighbors node (Overlay g1 g2))
    (if S.lookupLE node (nodes g1) == Just(node)
        then nodes g2
        else S.empty
    )
outNeighbors node graph = S.empty

{-
    List of nodes that point to the current node.
-}
inNeighbors :: Ord a => a -> AlgebraicGraph a -> S.Set a
inNeighbors node (Overlay g1 g2) = S.union (inNeighbors node g1) (inNeighbors node g2)
inNeighbors node (Connect g1 g2) = S.union (inNeighbors node (Overlay g1 g2))
    (if S.lookupLE node (nodes g2) == Just(node)
        then nodes g1
        else S.empty
    )
inNeighbors node graph = S.empty

{-
    Instantiate Num as a graph.

    > 1 :: AlgebraicGraph Int
    Node 1
    
    > 1*(2+3) :: AlgebraicGraph Int
    Connect (Node 1) (Overlay (Node 2) (Node 3))
-}
instance Num a => Num (AlgebraicGraph a) where
    fromInteger = Node . fromInteger
    (+) = Overlay
    (*) = Connect

{- 
    Instantiate show as a graph.
    
    > Node 1
    1

    > Connect (Node 1) (Overlay (Node 2) (Node 3))
    (1*(2+3))
-}
instance Show a => Show (AlgebraicGraph a) where
    show (Connect a b) = ['('] ++ (show a) ++ ['*'] ++ (show b) ++ [')']
    show (Overlay a b) = ['('] ++ (show a) ++ ['+'] ++ (show b) ++ [')']
    show (Node a) = show a
    show Empty = []

{-
    Instantiate Eq as a graph.

    Exemple:

    > Node 1 == 1
    True

    > Node 1 == 2
    False

    > angle == 1*2 + 1*3
    True

    > triangle == (1*2)*3
    True
-}
instance Ord a => Eq (AlgebraicGraph a) where
    g1 == g2 = (nodes g1 == nodes g2) && (edges g1 == edges g2)

{-
    Replace nodes with other graphs using the transformation function given.

    > extend (\n -> if n == 1 then 4+5 else Node n) $ 1*(2+3)
    ((4+5)*(2+3))
-}
extend :: (a -> AlgebraicGraph b) -> AlgebraicGraph a -> AlgebraicGraph b
extend f (Overlay a b) = Overlay (extend f a) (extend f b)
extend f (Connect a b) = Connect (extend f a) (extend f b)
extend f (Node a) = f a
extend f Empty = Empty

{-
    Split a node into a set of nodes by keeping the arcs involved.
-}

{-
    Helper func
-}
overlayNodes :: [a] -> AlgebraicGraph a
overlayNodes (ft:sd:tail) = Overlay (Node ft) (overlayNodes (sd:tail))
overlayNodes (head:tail) = Node head
overlayNodes [] = Empty

splitNode :: Eq a
          => a                 -- target
          -> [a]               -- replacements
          -> AlgebraicGraph a  -- current graph
          -> AlgebraicGraph a  -- new graph
splitNode node targets = extend (\n -> if n == node then overlayNodes targets else Node n)

{-
    Fmap is map for arbitrary structures.

    > fmap (+ 10) $ 1*(2+3) :: AlgebraicGraph Int
    (11*(12+13))
-}
instance Functor AlgebraicGraph where
    -- fmap :: (a -> b) -> AlgebraicGraph a -> AlgebraicGraph b
    fmap f graph = extend (\n -> Node (f n)) graph

{-
    Merge nodes that respect a given condition.
-}
mergeNodes :: (a -> Bool)       -- property
           -> a                 -- new node
           -> AlgebraicGraph a  -- current graph
           -> AlgebraicGraph a  -- new graph
mergeNodes prop node = fmap (\n -> if prop n then node else n)

{-
    Keep only the nodes that respect a given property.

    > nodes $ filterGraph odd $ 1*(2+3)
    fromList [1,3]

    > edges $ filterGraph odd $ 1*(2+3)
    fromList [(1,3)]
-}
filterGraph :: (a -> Bool) -> AlgebraicGraph a -> AlgebraicGraph a
filterGraph prop graph = extend (\n -> if prop n then Node n else Empty) graph

{-
    Remove a node from a graph.
-}
removeNode :: Eq a => a -> AlgebraicGraph a -> AlgebraicGraph a
removeNode node graph = filterGraph (\n -> n /= node) graph