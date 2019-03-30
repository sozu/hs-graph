{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeApplications #-}

module Data.Model.Graph.BaseSpec where

import Test.Hspec
import Control.Monad
import Data.Model.Graph.Base

data A = A Int deriving (Eq, Show)
data B = B Int deriving (Eq, Show)

spec :: Spec
spec = do
    let (a0, a1, a2) = (A 0, A 1, A 2)
    let (b0, b1, b2) = (B 0, B 1, B 2)
    let edge a b = Edge (Cursor b :: Cursor B) (Cursor a :: Cursor A) :: Edge B A

    describe "Basic graph operations" $ do
        it "values of new graph" $ do
            let g = newGraph :: Graph A :><: B :><: (B :- A)
            (values g :: [A]) `shouldBe` []
            (values g :: [B]) `shouldBe` []
            (values g :: [B :- A]) `shouldBe` ([] :: [Edge B A])

        it "replace nodes" $ do
            let g = newGraph :: Graph A :><: B
            let g' = replace g [a1, a2]
            (values g' :: [A]) `shouldBe` [a1, a2]
            (values g' :: [B]) `shouldBe` []

        it "values of graph" $ do
            let g = newGraph :: Graph A :><: B :><: (B :- A)
            let g' = replace (replace (replace g [a0]) [b0, b1, b2]) [edge 0 0, edge 0 2]
            (values g' :: [A]) `shouldBe` [a0]
            (values g' :: [B]) `shouldBe` [b0, b1, b2]
            let e00:[e02] = (values g' :: [B :- A])
            cursorIndex (edgeFrom e00) `shouldBe` 0
            cursorIndex (edgeTo e00) `shouldBe` 0
            cursorIndex (edgeFrom e02) `shouldBe` 2
            cursorIndex (edgeTo e02) `shouldBe` 0

    describe "Access to nodes" $ do
        it "valuesOf" $ do
            let g = newGraph :: Graph A :><: B
            let g' = replace (replace g [a0]) [b2, b1, b0]
            valuesOf @A g' `shouldBe` [a0]
            valuesOf @B g' `shouldBe` [b0, b1, b2]

        it "cursorsOf" $ do
            let g = newGraph :: Graph A :><: B
            let g' = replace (replace g [a0]) [b2, b1, b0]
            map cursorIndex (cursorsOf @A g') `shouldBe` [0]
            map cursorIndex (cursorsOf @B g') `shouldBe` [0, 1, 2]

        it "firstOf" $ do
            let g = newGraph :: Graph A :><: B :><: Int
            let g' = replace (replace g [a0]) [b2, b1, b0]
            cursorIndex <$> firstOf @A g' `shouldBe` Just 0
            cursorIndex <$> firstOf @B g' `shouldBe` Just 0
            cursorIndex <$> firstOf @Int g' `shouldBe` Nothing

        it "lastOf" $ do
            let g = newGraph :: Graph A :><: B :><: Int
            let g' = replace (replace g [a0]) [b2, b1, b0]
            cursorIndex <$> lastOf @A g' `shouldBe` Just 0
            cursorIndex <$> lastOf @B g' `shouldBe` Just 2
            cursorIndex <$> lastOf @Int g' `shouldBe` Nothing
