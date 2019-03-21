{-# language OverloadedLists #-}
module Main where

import Test.Hspec
import HaskellWorks.Hspec.Hedgehog
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import Algebra.Graph.AdjacencyMap (AdjacencyMap)

import qualified Algebra.Graph.AdjacencyMap.Algorithm as Graph
import qualified Algebra.Graph.Class as Graph
import qualified Algebra.Graph.NonEmpty.AdjacencyMap as NonEmpty
import qualified Data.Map as Map

import Action
import Movement
import Pos
import Thing
import ThingType

genPos :: MonadGen m => m Pos
genPos = Pos <$> Gen.int (Range.constant 0 100) <*> Gen.int (Range.constant 0 100)

genDir :: MonadGen m => m Dir
genDir = Gen.element [L, UL, U, UR, R, DR, D, DL]

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
            [ (TPlayer, (Pos 0 0, Just [Relative R]))
            , (TThing 0, (Pos 1 0, Just [Relative D]))
            , (TThing 1, (Pos 1 1, Just [Relative L]))
            , (TThing 2, (Pos 0 1, Just [Relative U]))
            ]

          graph :: AdjacencyMap Node
          graph = buildGraph freePositions actions

        Graph.scc graph `shouldBe`
          Graph.vertex
          (NonEmpty.edges1
           [ ( Node TPlayer (Pos 0 0) (Just $ Pos 0 1)
             , Node (TThing 2) (Pos 0 1) (Just $ Pos 0 0)
             )
           , ( Node (TThing 0) (Pos 1 0) (Just $ Pos 1 1)
             , Node TPlayer (Pos 0 0) (Just $ Pos 0 1)
             )
           , ( Node (TThing 1) (Pos 1 1) (Just $ Pos 0 1)
             , Node (TThing 0) (Pos 1 0) (Just $ Pos 1 1)
             )
           , ( Node (TThing 2) (Pos 0 1) (Just $ Pos 0 0)
             , Node (TThing 1) (Pos 1 1) (Just $ Pos 0 1)
             )
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
            [ (TPlayer, (Pos 0 0, Nothing))
            , (TThing 0, (Pos 1 0, Just [Relative D]))
            , (TThing 1, (Pos 1 1, Just [Relative L]))
            , (TThing 2, (Pos 0 1, Just [Relative U]))
            ]

          graph :: AdjacencyMap Node
          graph = buildGraph freePositions actions

        Graph.scc graph `shouldBe`
          Graph.edges
          [ ( NonEmpty.vertex $ Node TPlayer (Pos 0 0) Nothing
            , NonEmpty.vertex $ Node (TThing 2) (Pos 0 1) (Just $ Pos 0 0)
            )
          , ( NonEmpty.vertex $ Node (TThing 1) (Pos 1 1) (Just $ Pos 0 1)
            , NonEmpty.vertex $ Node (TThing 0) (Pos 1 0) (Just $ Pos 1 1)
            )
          , ( NonEmpty.vertex $ Node (TThing 2) (Pos 0 1) (Just $ Pos 0 0)
            , NonEmpty.vertex $ Node (TThing 1) (Pos 1 1) (Just $ Pos 0 1)
            )
          ]

        runMoves graph `shouldBe` mempty
      it "moving to the same place D DL" $ do
        let freePositions = const True
        let
          actions =
            Map.fromList
            [ (TPlayer, (Pos 0 0, Just [Relative D]))
            , (TThing 0, (Pos 1 0, Just [Relative DL]))
            ]

          graph :: AdjacencyMap Node
          graph = buildGraph freePositions actions

        Graph.scc graph `shouldBe`
          Graph.vertices
          [ NonEmpty.vertex $ Node TPlayer (Pos 0 0) (Just $ Pos 0 1)
          , NonEmpty.vertex $ Node (TThing 0) (Pos 1 0) (Just $ Pos 0 1)
          ]

        runMoves graph `shouldBe` Map.singleton TPlayer (Pos 0 1)
      it "prop - moving to the same place" $
        require prop_sameDestination

prop_sameDestination :: Property
prop_sameDestination =
  property $ do
    p <- forAll genPos
    d1 <- forAll genDir
    d2 <- forAll $ Gen.filter (/=d1) genDir
    let p1 = runMove' p (Relative $ reverseDir d1)
    let p2 = runMove' p (Relative $ reverseDir d2)

    let freePositions = const True
    let
      {-

      P(x,y)--> <--T(x+2,y)

      -}
      actions =
        Map.fromList
        [ (TPlayer, (p1, Just [Relative d1]))
        , (TThing 0, (p2, Just [Relative d2]))
        ]

      graph :: AdjacencyMap Node
      graph = buildGraph freePositions actions

    Graph.scc graph ===
      Graph.vertices
      [ NonEmpty.vertex $ Node TPlayer p1 (Just p)
      , NonEmpty.vertex $ Node (TThing 0) p2 (Just p)
      ]

    runMoves graph === Map.singleton TPlayer p
