{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Control.Monad.State
import Data.Model.Graph

data A = A { a1 :: Int, a2 :: String } deriving (Show)
data B = B { b1 :: Int, b2 :: String } deriving (Show)

type AB = Graph A :><: B :><: (B :- A)

main :: IO ()
main = do
    (_, graph) <- flip runStateT (newGraph :: AB) $ do
        replicateM_ 10000 $ do
            --(+<<) (A 1 "a")
            ca <- (+<<) (A 1 "a")
            replicateM_ 10 $ do
                cb <- (+<<) (B 2 "b")
                cb -*< ca
    print $ sum $ map a1 $ valuesOf @A graph
    --print $ sum $ map b1 $ valuesOf @B graph