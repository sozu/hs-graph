{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Data.Model.Graph.Operators (
    (@<)
    , (@*<) , (*@<)
    , (+<) , (>+) , (/<) , (~<)
    , (@<<)
    , (@*<<) , (*@<<)
    , (+<<) , (/<<) , (~<<)
    , (?<<)
    , (-*)
    , (-*<)
) where

import Data.List as L
import Data.Model.Graph.Base
import Control.Monad.State

-- Cursor operators

-- | Get a value by a cursor.
(@<) :: (GraphContainer g a)
     => CursorT a rs -- ^ A cursor indicating a value.
     -> g -- ^ A graph.
     -> a -- ^ A value in the graph indicated by the cursor.
(@<) c graph = vs !! (length vs - 1 - cursorIndex c)
    where
        vs = values graph

-- | Get cursors related by a cursor.
(@*<) :: (GraphContainer g (EdgeT a b rs))
      => CursorT a rs -- ^ A cursor having relations to cursors to be returned.
      -> g -- ^ A graph.
      -> [Cursor b] -- ^ Cursors related by the cursor.
(@*<) c graph = map edgeTo $ _edgesFrom c graph

-- | Get cursors related to a cursor.
(*@<) :: (GraphContainer g (EdgeT b a rs))
      => CursorT a rs -- ^ A cursor related by cursors to be returned.
      -> g -- ^ A graph.
      -> [Cursor b] -- ^ Cursors related to the cursor.
(*@<) c graph = map ((-|) . edgeFrom) $ _edgesTo c graph

-- | Add a value into a graph.
(+<) :: (GraphContainer g a)
     => a -- ^ A value to add.
     -> g -- ^ A graph.
     -> (g, Cursor a) -- | A graph modified by this addition and a cursor indicating new value.
(+<) v graph = (replace graph (v:vs), Cursor (length vs))
    where
        vs = values graph

-- | Add a value into a graph (flipped version of (+<)).
(>+) :: (GraphContainer g a)
     => g -- ^ A graph.
     -> a -- ^ A value to add.
     -> (g, Cursor a) -- | A graph modified by this addition and a cursor indicating new value.
(>+) = flip (+<)

-- | Replace a value indicated by a cursor.
(/<) :: (GraphContainer g a)
     => a -- ^ New value.
     -> CursorT a rs -- ^ A cursor indicating a value to replace.
     -> (g -> g) -- ^ A function which executes the replacement in the graph of an argument and returns modified graph.
(/<) v c = \graph ->
        let vs = values graph
            index = length vs - 1 - cursorIndex c
        in replace graph $ take index vs ++ v:(drop (index+1) vs)

-- | Update a value indicated by a cursor.
(~<) :: (GraphContainer g a)
     => (a -> a) -- ^ A function to change current value to new value.
     -> CursorT a rs -- ^ A cursor indicating a value to update.
     -> (g -> g) -- ^ A function which executes the update in the graph of an argument and returns modified graph.
(~<) f c = \graph -> f (c @< graph) /< c $ graph

-- Cursor operators in state monad

-- | Get a value by a cursor in state monad.
(@<<) :: (GraphContainer g a, Monad m)
      => CursorT a rs -- ^ A cursor indicating a value.
      -> StateT g m a -- ^ A value in the graph indicated by the cursor.
(@<<) c = get >>= return . (@<) c

-- | Get cursors related by a cursor in state monad.
(@*<<) :: forall b rs a g m. (GraphContainer g (EdgeT a b rs), Monad m)
       => CursorT a rs -- ^ A cursor having relations to cursors to be returned.
       -> StateT g m [Cursor b] -- ^ Cursors related by the cursor.
(@*<<) c = get >>= return . (@*<) c

-- | Get cursors related to a cursor in state monad.
(*@<<) :: forall b rs a g m. (GraphContainer g (EdgeT b a rs), Monad m)
       => CursorT a rs -- ^ A cursor related by cursors to be returned.
       -> StateT g m [Cursor b] -- ^ Cursors related to the cursor.
(*@<<) c = get >>= return . (*@<) c

-- | Add a value into a graph in state monad.
(+<<) :: (GraphContainer g a, Monad m)
      => a -- ^ A value to add.
      -> StateT g m (Cursor a) -- ^ A cursor indicating new value.
(+<<) v = do
    g <- get
    let (g', c) = v +< g
    put g'
    return c

-- | Replace a value of a graph in state monad.
(/<<) :: (GraphContainer g a, Monad m)
      => a -- ^ New value.
      -> CursorT a rs -- ^ A cursor indicating a value to replace.
      -> StateT g m () -- ^ No value.
(/<<) v c = get >>= \graph -> put $ (v /< c) graph

-- | Update a value of a graph in state monad.
(~<<) :: (GraphContainer g a, Monad m)
      => (a -> a) -- ^ A function to change current value to new value.
      -> CursorT a rs -- ^ A cursor indicating a value to update.
      -> StateT g m () -- ^ No value.
(~<<) v c = get >>= \graph -> put $ (v ~< c) graph

-- Traversal

(?<<) :: forall g a m. (GraphContainer g a, Monad m)
      => (a -> Bool)
      -> StateT g m (Maybe (Cursor a))
(?<<) f = do
      graph <- get
      return $ L.findIndex f (valuesOf @a graph) >>= (Just . Cursor)

-- Edge operators

-- | Create an edge between cursors.
(-*) :: (GraphContainer g (EdgeT a b rs))
     => g -- ^ A graph.
     -> CursorT a rs -- ^ Edge should be created from this cursor.
     -> CursorT b qs -- ^ Edge should be created to this cursor.
     -> g -- ^ Modifed graph holding a created edge.
(-*) graph ca cb = fst $ Edge ca ((-|) cb) +< graph

-- Edge operators in state monad

-- | Create an edge between cursors in state monad.
(-*<) :: (GraphContainer g (EdgeT a b rs), Monad m)
      => CursorT a rs -- ^ Edge should be created from this cursor.
      -> CursorT b qs -- ^ Edge should be created to this cursor.
      -> StateT g m () -- ^ No value.
(-*<) ca cb = do
    graph <- get
    put $ (-*) graph ca cb

-- Private

_edgesFrom :: (GraphContainer g (EdgeT a b rs))
           => CursorT a rs
           -> g
           -> [EdgeT a b rs]
_edgesFrom c graph = (filter match (valuesOf graph))
    where
        match :: EdgeT a b rs -> Bool
        match edge = cursorIndex (edgeFrom edge) == cursorIndex c

_edgesTo :: (GraphContainer g (EdgeT b a rs))
         => CursorT a rs
         -> g
         -> [EdgeT b a rs]
_edgesTo c graph = (filter match (valuesOf graph))
    where
        match :: EdgeT b a rs -> Bool
        match edge = cursorIndex (edgeTo edge) == cursorIndex c
