{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeApplications #-}

module Data.Model.Graph.OperatorsSpec where

import Test.Hspec
import Control.Monad
import Control.Monad.State
import Data.Model.Graph.Base
import Data.Model.Graph.Operators

data A = A Int deriving (Eq, Show)
data B = B Int deriving (Eq, Show)

type AB = Graph A :><: B :><: (B :- A)

spec :: Spec
spec = do
    let (a0, a1, a2) = (A 0, A 1, A 2)
    let (b0, b1, b2) = (B 0, B 1, B 2)
    let edge a b = Edge (Cursor b :: Cursor B) (Cursor a :: Cursor A) :: Edge B A

    describe "Basic graph operators" $ do
        it "Get node" $ do
            let g = replace (newGraph :: AB) [a0, a1, a2]
            (Cursor 1 :: Cursor A) @< g `shouldBe` a1

        it "Get nodes related by a cursor" $ do
            let g = replace (replace (replace (newGraph :: AB) [a0, a1, a2]) [b0, b1, b2]) [edge 2 1, edge 1 0, edge 0 0]
            map cursorIndex ((Cursor 0 :: Cursor B) @*< g :: [Cursor A]) `shouldBe` [0, 1]
            map cursorIndex ((Cursor 1 :: Cursor B) @*< g :: [Cursor A]) `shouldBe` [2]
            map cursorIndex ((Cursor 2 :: Cursor B) @*< g :: [Cursor A]) `shouldBe` []

        it "Get nodes related to a cursor" $ do
            let g = replace (replace (replace (newGraph :: AB) [a0, a1, a2]) [b0, b1, b2]) [edge 1 2, edge 0 1, edge 0 0]
            map cursorIndex ((Cursor 0 :: Cursor A) *@< g :: [Cursor B]) `shouldBe` [0, 1]
            map cursorIndex ((Cursor 1 :: Cursor A) *@< g :: [Cursor B]) `shouldBe` [2]
            map cursorIndex ((Cursor 2 :: Cursor A) *@< g :: [Cursor B]) `shouldBe` []

        it "Add a value" $ do
            let g = newGraph :: AB
            let g' = fst $ a0 +< g 
            valuesOf @A g' `shouldBe` [a0]
            valuesOf @A (fst $ a1 +< g') `shouldBe` [a0, a1]

        it "Replace a value" $ do
            let g = fst $ a2 +< (fst $ a1 +< (fst $ a0 +< (newGraph :: AB)))
            let r = A 3 /< (Cursor 1 :: Cursor A)
            valuesOf @A (r g) `shouldBe` [a0, A 3, a2]

        it "Update a value" $ do
            let g = fst $ a2 +< (fst $ a1 +< (fst $ a0 +< (newGraph :: AB)))
            let u = (\(A v) -> A (v + 10)) ~< (Cursor 1 :: Cursor A)
            valuesOf @A (u g) `shouldBe` [a0, A 11, a2]

    describe "Basic operators in state monad" $ do
        it "Get node" $ do
            let g = replace (newGraph :: AB) [a0, a1, a2]
            (v, _) <- (`runStateT` g) $ do
                (@<<) (Cursor 1 :: Cursor A)
            v `shouldBe` a1

        it "Get nodes related by a cursor" $ do
            let g = replace (replace (replace (newGraph :: AB) [a0, a1, a2]) [b0, b1, b2]) [edge 2 1, edge 1 0, edge 0 0]
            (`runStateT` g) $ do
                v0 <- (@*<<) @A (Cursor 0 :: Cursor B)
                v1 <- (@*<<) @A (Cursor 1 :: Cursor B)
                v2 <- (@*<<) @A (Cursor 2 :: Cursor B)
                liftIO $ do
                    map cursorIndex v0 `shouldBe` [0, 1]
                    map cursorIndex v1 `shouldBe` [2]
                    map cursorIndex v2 `shouldBe` []
            return () :: IO ()

        it "Get nodes related to a cursor" $ do
            let g = replace (replace (replace (newGraph :: AB) [a0, a1, a2]) [b0, b1, b2]) [edge 1 2, edge 0 1, edge 0 0]
            (`runStateT` g) $ do
                v0 <- (*@<<) @B (Cursor 0 :: Cursor A)
                v1 <- (*@<<) @B (Cursor 1 :: Cursor A)
                v2 <- (*@<<) @B (Cursor 2 :: Cursor A)
                liftIO $ do
                    map cursorIndex v0 `shouldBe` [0, 1]
                    map cursorIndex v1 `shouldBe` [2]
                    map cursorIndex v2 `shouldBe` []
            return () :: IO ()

        it "Add a value" $ do
            let g = newGraph :: AB
            (`runStateT` g) $ do
                (+<<) a0
                get >>= \g' -> liftIO (valuesOf @A g' `shouldBe` [a0])
                (+<<) a1
                get >>= \g' -> liftIO (valuesOf @A g' `shouldBe` [a0, a1])
                (+<<) a2
                get >>= \g' -> liftIO (valuesOf @A g' `shouldBe` [a0, a1, a2])
            return () :: IO ()

        it "Replace a value" $ do
            let g = newGraph :: AB
            (`runStateT` g) $ do
                c0 <- (+<<) a0
                A 3 /<< c0
                get >>= \g' -> liftIO (valuesOf @A g' `shouldBe` [A 3])
                c1 <- (+<<) a1
                A 4 /<< c1
                get >>= \g' -> liftIO (valuesOf @A g' `shouldBe` [A 3, A 4])
            return () :: IO ()

        it "Update a value" $ do
            let g = newGraph :: AB
            let u = \(A v) -> A (v + 10)
            (`runStateT` g) $ do
                c0 <- (+<<) a0
                u ~<< c0
                get >>= \g' -> liftIO (valuesOf @A g' `shouldBe` [A 10])
                c1 <- (+<<) a1
                u ~<< c1
                get >>= \g' -> liftIO (valuesOf @A g' `shouldBe` [A 10, A 11])
            return () :: IO ()

    describe "Edge operators" $ do
        it "Add edge" $ do
            let g = replace (replace (newGraph :: AB) [a0, a1, a2]) [b0, b1, b2]
            let g' = (-*) g (Cursor 0 :: Cursor B) (Cursor 0 :: Cursor A)
            let g'' = (-*) g' (Cursor 1 :: Cursor B) (Cursor 1 :: Cursor A)
            let es = valuesOf @(B :- A) g''
            cursorIndex (edgeFrom (es !! 0)) `shouldBe` 0
            cursorIndex (edgeFrom (es !! 1)) `shouldBe` 1
            cursorIndex (edgeTo (es !! 0)) `shouldBe` 0
            cursorIndex (edgeTo (es !! 1)) `shouldBe` 1

        it "Add edge in state monad" $ do
            let g = newGraph :: AB
            (_, g') <- (`runStateT` g) $ do
                ca0 <- (+<<) a0
                ca1 <- (+<<) a1
                cb0 <- (+<<) b0
                cb1 <- (+<<) b1
                cb0 -*< ca0
                cb1 -*< ca1
            let es = valuesOf @(B :- A) g'
            cursorIndex (edgeFrom (es !! 0)) `shouldBe` 0
            cursorIndex (edgeFrom (es !! 1)) `shouldBe` 1
            cursorIndex (edgeTo (es !! 0)) `shouldBe` 0
            cursorIndex (edgeTo (es !! 1)) `shouldBe` 1

    describe "Traversal" $ do
        it "Get first cursor satisfying predicate" $ do
            let g = newGraph :: AB
            (c, _) <- (`runStateT` g) $ do
                (+<<) a0
                (+<<) a1
                (+<<) a2
                (?<<) (\(A v) -> odd v)
            cursorIndex <$> c `shouldBe` Just 1

        it "When no node satisfies predicate" $ do
            let g = newGraph :: AB
            (c, _) <- (`runStateT` g) $ do
                (+<<) a0
                (+<<) a2
                (?<<) (\(A v) -> odd v)
            cursorIndex <$> c `shouldBe` Nothing