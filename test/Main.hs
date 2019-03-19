{-# language OverloadedLists #-}
module Main where

import Test.Hspec

import Algebra.Graph.AdjacencyMap (AdjacencyMap)

import qualified Algebra.Graph.AdjacencyMap.Algorithm as Graph
import qualified Algebra.Graph.Class as Graph
import qualified Algebra.Graph.NonEmpty.AdjacencyMap as NonEmpty
import qualified Data.Dependent.Map as DMap
import qualified Data.Map as Map

import Action
import Movement
import Pos
import ThingType

main :: IO ()
main =
  hspec $ do
    describe "move resolution" $ do
      it "4-cycle" $ do
        let freePositions = const True
        let
          {-

          P(0,0) -->  T0(1,0)
            ^           |
            |           v
           T2(0,1) <--  T1(1,1)

          -}
          actions =
            Map.fromList
            [ (TPlayer, (Pos 0 0, DMap.singleton (Move R) (pure 1)))
            , (TThing 0, (Pos 1 0, DMap.singleton (Move D) (pure 1)))
            , (TThing 1, (Pos 1 1, DMap.singleton (Move L) (pure 1)))
            , (TThing 2, (Pos 0 1, DMap.singleton (Move U) (pure 1)))
            ]

          graph :: AdjacencyMap Node
          graph = buildGraph freePositions actions

        Graph.scc graph `shouldBe`
          Graph.vertex
          (NonEmpty.edges1
           [ (Node TPlayer (Just $ Pos 0 1), Node (TThing 2) (Just $ Pos 0 0))
           , (Node (TThing 0) (Just $ Pos 1 1), Node TPlayer (Just $ Pos 0 1))
           , (Node (TThing 1) (Just $ Pos 0 1), Node (TThing 0) (Just $ Pos 1 1))
           , (Node (TThing 2) (Just $ Pos 0 0), Node (TThing 1) (Just $ Pos 0 1))
           ])

        runMoves graph `shouldBe`
          Map.fromList
          [ (TPlayer, Pos 1 0)
          , (TThing 0, Pos 1 1)
          , (TThing 1, Pos 0 1)
          , (TThing 2, Pos 0 0)
          ]
      it "4-cycle where one doesn't move" $ do
        let freePositions = const True
        let
          {-

          P(0,0)      T0(1,0)
            ^           |
            |           v
           T2(0,1) <--  T1(1,1)

          -}
          actions =
            Map.fromList
            [ (TPlayer, (Pos 0 0, DMap.singleton Wait (pure ())))
            , (TThing 0, (Pos 1 0, DMap.singleton (Move D) (pure 1)))
            , (TThing 1, (Pos 1 1, DMap.singleton (Move L) (pure 1)))
            , (TThing 2, (Pos 0 1, DMap.singleton (Move U) (pure 1)))
            ]

          graph :: AdjacencyMap Node
          graph = buildGraph freePositions actions

        Graph.scc graph `shouldBe`
          Graph.edges
          [ ( NonEmpty.vertex $ Node TPlayer Nothing
            , NonEmpty.vertex $ Node (TThing 2) (Just $ Pos 0 0)
            )
          , ( NonEmpty.vertex $ Node (TThing 1) (Just $ Pos 0 1)
            , NonEmpty.vertex $ Node (TThing 0) (Just $ Pos 1 1)
            )
          , ( NonEmpty.vertex $ Node (TThing 2) (Just $ Pos 0 0)
            , NonEmpty.vertex $ Node (TThing 1) (Just $ Pos 0 1)
            )
          ]

        runMoves graph `shouldBe` mempty
