{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE IncoherentInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE GADTs #-}

module Data.Model.Graph.Base (
    Graph(..)
    , (:><:)
    , GraphFactory(..)
    , GraphContainer(..)
    , CursorT(..)
    , Cursor
    , (+|), (-|)
    , EdgeT(..)
    , Edge
    , (:-)
    , (:+|)
    , firstOf, lastOf
    , Serialize
    , serialize
    , serializeCursor
) where

import Data.Proxy

-- | Base type used for the declaration of a graph type.
-- Although this type has just a list of a type, following type operator enables the graph to hold values of multiple types.
data Graph a = Graph [a]

-- | A type operator which defines a new graph type by adding a list of the type denoted by type parameter of the right.
-- This operator can be applied in series to define a graph holding lists of multiple types. i.e.,
--
-- > type MyGraph = Graph A :><: B :><: C
--
-- 'MyGraph' can hold values of B and C in addition to A.
data g :><: a = (:><:) g [a]

-- | This class declares a method to create a graph object.
-- You should use this method instead of value constructor to generate inner lists holding values of multiple types.
class GraphFactory g where
    -- | Create a new graph object.
    -- This method is designed to be invoked with explicit type signature. i.e.,
    --
    -- > let graph = newGraph :: MyGraph
    newGraph :: g

instance GraphFactory (Graph a) where
    newGraph = Graph []

instance (GraphFactory g) => GraphFactory (g :><: a) where
    newGraph = newGraph :><: ([] :: [a])

-- | Cursor indicates a value in a graph.
-- Unused type parameter denotes the type of a value indicated by this cursor.
-- 
-- Currently, cursor is just an index of the inner list.
-- This feature may be changed to enable convenient functions, i.e.,
-- - Removal of values from a graph.
-- - Preventing using cursor to a graph other than the graph it is obtained.
--data CursorT a rs = Cursor Int deriving (Show)

type Cursor a = CursorT a '[]

data CursorT a (rs :: [*]) where
    Cursor :: Int -> CursorT a '[]
    CursorT :: Int -> CursorT a rs

instance Show (CursorT a rs) where
    show (Cursor index) = "Cursor " ++ show index
    show (CursorT index) = "Cursor " ++ show index

(+|) :: Proxy (rs :: [*])
     -> Cursor a
     -> CursorT a rs
(+|) _ (Cursor index) = CursorT index

(-|) :: CursorT a rs
     -> Cursor a
(-|) (Cursor index) = Cursor index
(-|) (CursorT index) = Cursor index

-- | GraphContainer represents the relation that graph g contains nodes of type a.
class (GraphFactory g) => GraphContainer g a where
    -- | Extracts nodes of a annotated type.
    -- This method should be invoked with explicit type annotation like below.
    --
    -- > let vs = values graph :: [A]
    values :: g -- ^ A graph.
           -> [a] -- ^ Nodes of a annotated type.

    -- | Replaces all nodes of a type and returns modified graph.
    replace :: g -- ^ A graph.
            -> [a] -- ^ Node of type a.
            -> g -- ^ Modified graph.

-- 親がGraph aならばaのコンテナを持つという実装。
instance GraphContainer (Graph a) a where
    values (Graph vs) = vs
    replace (Graph _) vs = Graph vs

-- 子がbならば、bのコンテナを持つという実装。
instance (GraphFactory a) => GraphContainer (a :><: b) b where
    values ((:><:) _ vs) = vs
    replace ((:><:) graph _) vs = graph :><: vs

-- 親がaのコンテナを持つ何かならば、aのコンテナを持つという実装。
instance (GraphContainer g a) => GraphContainer (g :><: b) a where
    values ((:><:) graph _) = values graph
    replace ((:><:) parent graph) vs = replace parent vs :><: graph

firstOf :: forall g a. (GraphContainer g a)
        => g
        -> Maybe (Cursor a)
firstOf graph = case length (values graph :: [a]) of
                0 -> Nothing
                v -> Just (Cursor 0 :: Cursor a)

lastOf :: forall g a. (GraphContainer g a)
       => g
       -> Maybe (Cursor a)
lastOf graph = case length (values graph :: [a]) of
                0 -> Nothing
                v -> Just (Cursor (v-1) :: Cursor a)

-- | Edge is defined by two cursors each of which is an endpoint of the edge.
--data Edge a b = Edge { edgeFrom :: Cursor a, edgeTo :: Cursor b }
--              | Edge' a b
--              deriving (Show)

-- | Type operator representing an edge from left type to right type.
type (a :- b) = Edge a b

type Edge a b = EdgeT a b '[]

data Relations 

data EdgeT a b (rs :: [*]) where
    Edge :: { edgeFrom :: CursorT a rs
            , edgeTo :: Cursor b
            } -> EdgeT a b rs
    Edge' :: a -> b -> Edge a b
    EdgeT' :: a -> b -> Proxy rs -> EdgeT a b rs

instance Show (EdgeT a b rs) where
    show e = "(" ++ show (edgeFrom e) ++ ", " ++ show (edgeTo e) ++ ")"

type family (:+|) a b :: * where
    (:+|) (EdgeT a b rs) r = EdgeT a b (r ': rs)
    (:+|) (CursorT a rs) r = CursorT a (r ': rs)

-- | Serialize extracts all node types in the graph in the order of dependencies.
type family Serialize g :: [*] where
    Serialize g = Sort (Serialize' g) g

-- | Extract nodes of the first type of types listed in proxy argument.
-- This function also returns a proxy of subsequent types which is available for looping operation.
serialize :: (GraphContainer g a)
          => g -- ^ A graph.
          -> proxy (a ': xs) -- ^ A proxy of types.
          -> ([a], Proxy xs) -- ^ Nodes of the first type and a proxy of subsequent types.
serialize graph p = (values graph, Proxy :: Proxy xs)

-- | Like serialize, but returns cusrors instead of nodes.
serializeCursor :: (GraphContainer g a)
                => g -- ^ A graph.
                -> proxy (a ': xs) -- ^ A proxy of types.
                -> ([Cursor a], Proxy xs) -- ^ Cursors of the first type and a proxy of subsequent types.
serializeCursor graph p = (map (\i -> Cursor i :: Cursor a) [0..(length vs - 1)], p')
    where (vs, p') = serialize graph p

-- Private

type family Serialize' g :: [*] where
    Serialize' (xs :><: EdgeT a b rs) = Serialize' xs
    Serialize' (xs :><: x) = Append x (Serialize' xs)
    Serialize' (EdgeT a b rs) = '[]
    Serialize' (Graph x) = '[x]
    Serialize' x = '[x]

type family Sort as g :: [*] where
    Sort (x ': xs) g = Sort' x xs g (HasEdgeIn x xs g)

type family Sort' a as g (b :: Bool) :: [*] where
    Sort' x (y ': ys) g 'True = Sort' y (Append x ys) g (HasEdgeIn y (Append x ys) g)
    Sort' x (y ': ys) g 'False = x ': Sort' y ys g (HasEdgeIn y ys g)
    Sort' x '[] g _ = '[x]

type family Append a as :: [k] where
    Append x '[] = '[x]
    Append x (y ': ys) = y ': Append x ys

type family HasEdgeIn a as g :: Bool where
    HasEdgeIn x (y ': ys) g = Or (HasEdge x y g) (HasEdgeIn x ys g)
    HasEdgeIn x '[] g = 'False

type family HasEdge a b g :: Bool where
    HasEdge y z (Graph x :><: xs) = HasEdge y z xs 
    HasEdge y z (EdgeT y z rs :><: xs) = 'True
    HasEdge y z (x :><: xs) = HasEdge y z xs
    HasEdge y z (EdgeT y z rs) = 'True
    HasEdge y z x = 'False

type family Or (a :: Bool) (b :: Bool) :: Bool where
    Or 'True _ = 'True
    Or 'False b = b

--type family Edges g :: [Edge * *] where
--    Edges (xs :><: Edge a b) = Append b (Edges xs)
--    Edges (xs :><: x) = Edges xs
--    Edges (Edge a b) = '[b]
--    Edges _ = '[]
--
--type family EdgesFrom g (a :: *) :: [*] where
--    EdgesFrom (xs :><: Edge a b) a = Append b (EdgesFrom xs a)
--    EdgesFrom (xs :><: x) a = EdgesFrom xs a
--    EdgesFrom (Edge a b) a = '[b]
--    EdgesFrom _ a = '[]