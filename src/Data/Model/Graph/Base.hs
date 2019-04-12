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
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeApplications #-}

module Data.Model.Graph.Base (
    Graph(..)
    , (:><:)
    , (:++:)
    , GraphFactory(..)
    , GraphContainer(..)
    , CursorT(..)
    , Cursor
    , (+|), (-|)
    , cursorIndex
    , EdgeT(..)
    , Edge
    , (:-)
    , (:+|)
    , valuesOf, cursorsOf, cursorsOf'
    , reversedValuesOf
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

-- | Concatenates graphs or nodes of graphs.
-- If RHS type is a @Graph a@, its nodes are extracted and concatenated to LHS type.
--
-- > Graph a :++: Graph b == Graph a :><: b
-- > Graph a :><: b :++: Graph c == Graph a :><: b :><: c
-- > Graph a :++: (Graph b :><: c) == Graph a :><: b :><: c
-- > Graph a :++: (b :><: c) == Graph a :><: b :><: c
type family (:++:) (a :: *) (b :: *) where
    a :++: (bs :><: b)       = a :++: bs :><: b
    a :++: (Graph b)         = a :><: b
    a :++: b                 = a :><: b

infixl 4 :><:
infixl 4 :++:

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

-- | This type holds the index of a value contained in a graph.
-- @a@ Denotes the type of the value.
-- @rs@ are qualified types available for any purpose to supply additional information to the value type.
data CursorT a (rs :: [*]) where
    Cursor :: Int -> CursorT a '[]
    CursorT :: Int -> CursorT a rs

type Cursor a = CursorT a '[]

instance Show (CursorT a rs) where
    show (Cursor index) = "Cursor " ++ show index
    show (CursorT index) = "Cursor " ++ show index

-- | Returns an index of the cursor.
cursorIndex :: CursorT a rs -- ^ A cursor.
            -> Int -- ^ The index of the cursor.
cursorIndex (Cursor index) = index
cursorIndex (CursorT index) = index

-- | Qualifies a cursor with additional informative types.
(+|) :: Proxy (rs :: [*]) -- ^ Additional types.
     -> Cursor a -- ^ A cursor.
     -> CursorT a rs -- ^ Qualified cursor.
(+|) _ (Cursor index) = CursorT index

-- | Removes additional informative types from a cursor.
(-|) :: CursorT a rs -- ^ A cursor.
     -> Cursor a -- ^ Unqualified cursor.
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

instance GraphContainer (Graph a) a where
    values (Graph vs) = vs
    replace (Graph _) vs = Graph vs

instance (GraphFactory a) => GraphContainer (a :><: b) b where
    values ((:><:) _ vs) = vs
    replace ((:><:) graph _) vs = graph :><: vs

instance (GraphContainer g a) => GraphContainer (g :><: b) a where
    values ((:><:) graph _) = values graph
    replace ((:><:) parent graph) vs = replace parent vs :><: graph

-- | Obtains values of the specified type from a graph.
valuesOf :: forall a g. (GraphContainer g a)
         => g -- ^ A graph.
         -> [a] -- ^ Values of @a@.
valuesOf graph = reverse $ values graph :: [a]

-- | Obtains values of the specified type from a graph in reversed order.
--
-- This function is faster than @valuesOf@ because values are stored in graph in reversed order.
reversedValuesOf :: forall a g. (GraphContainer g a)
                 => g -- ^ A graph.
                 -> [a] -- ^ Values of @a@.
reversedValuesOf = values

-- | Obtains cursors of the specified type from a graph.
cursorsOf :: forall a rs g. (GraphContainer g a)
          => g -- ^ A graph.
          -> [CursorT a rs] -- ^ Cursors of @a@.
cursorsOf graph = map ((Proxy :: Proxy rs) +|) $ fst $ serializeCursor graph (Proxy :: Proxy '[a])

-- | Obtains unqualified cursors of the specified type from a graph.
cursorsOf' :: forall a g. (GraphContainer g a)
           => g -- ^ A graph.
           -> [Cursor a] -- ^ Unqualified cursors of @a@.
cursorsOf' graph = fst $ serializeCursor graph (Proxy :: Proxy '[a])

-- | Returns the cursor for the first value of the specified type from a graph.
firstOf :: forall a g. (GraphContainer g a)
        => g -- ^ A graph.
        -> Maybe (Cursor a) -- ^ The cursor for the first value if exists.
firstOf graph = case length (values graph :: [a]) of
                0 -> Nothing
                v -> Just (Cursor 0 :: Cursor a)

-- | Returns the cursor for the last value of the specified type from a graph.
lastOf :: forall a g. (GraphContainer g a)
       => g -- ^ A graph.
       -> Maybe (Cursor a) -- ^ The cursor for the last value if exists.
lastOf graph = case length (values graph :: [a]) of
                0 -> Nothing
                v -> Just (Cursor (v-1) :: Cursor a)

-- | Type operator representing an edge from left type to right type.
type (a :- b) = Edge a b

infixl 5 :-

type Edge a b = EdgeT a b '[]

-- | This type represents an edge between two nodes in a graph.
-- @a@ and @b@ denote data types of the nodes.
-- @rs@ are qualified types available for any purpose to supply additional information to the edge.
data EdgeT a b (rs :: [*]) where
    Edge :: { edgeFrom :: CursorT a rs
            , edgeTo :: Cursor b
            } -> EdgeT a b rs

instance Eq (EdgeT a b rs) where
    e1 == e2 = (cursorIndex (edgeFrom e1) == cursorIndex (edgeFrom e2)) && (cursorIndex (edgeTo e1) == cursorIndex (edgeTo e2))

instance Show (EdgeT a b rs) where
    show e = "(" ++ show (edgeFrom e) ++ ", " ++ show (edgeTo e) ++ ")"

-- | Qualifies an edge type with additional informative type.
type family (:+|) a b :: * where
    (:+|) (EdgeT a b rs) r = EdgeT a b (r ': rs)
    (:+|) (CursorT a rs) r = CursorT a (r ': rs)

infixl 4 :+|

-- | Serialize extracts all node types in the graph in the order of dependencies.
type family Serialize g :: [*] where
    Serialize g = Sort (Serialize' g) g

-- | Extract nodes of the first type of types listed in proxy argument.
-- This function also returns a proxy of subsequent types which is available for looping operation.
serialize :: forall g a proxy xs. (GraphContainer g a)
          => g -- ^ A graph.
          -> proxy (a ': xs) -- ^ A proxy of types.
          -> ([a], Proxy xs) -- ^ Nodes of the first type and a proxy of subsequent types.
serialize graph p = (valuesOf @a graph, Proxy :: Proxy xs)

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