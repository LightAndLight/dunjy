{-# language OverloadedLists #-}
module Main where

import Test.Hspec
import HaskellWorks.Hspec.Hedgehog
import Hedgehog (Property, MonadGen, (===), property, forAll)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import Algebra.Graph.AdjacencyMap (AdjacencyMap)
import Control.Lens.Setter ((?~))
import Control.Monad.State (evalState)
import Data.Function ((&))
import Data.Functor.Identity (Identity)
import Data.Map (Map)
import Data.Map.Monoidal (MonoidalMap(..))
import Reflex.Class (never)
import Reflex.Spider.Internal (Spider)

import qualified Algebra.Graph.AdjacencyMap.Algorithm as Graph
import qualified Algebra.Graph.Class as Graph
import qualified Algebra.Graph.NonEmpty.AdjacencyMap as NonEmpty
import qualified Data.Map as Map

import Action
import Dunjy
import Movement
import Pos
import Thing
import ThingType

genPos :: MonadGen m => m Pos
genPos = Pos <$> Gen.int (Range.constant 0 100) <*> Gen.int (Range.constant 0 100)

genDir :: MonadGen m => m Dir
genDir = Gen.element [L, UL, U, UR, R, DR, D, DL]

runMoves :: Map ThingType (Thing t Identity) -> Map ThingType Action -> Map ThingType (Updates t)
runMoves mobs actions =
  getMonoidalMap $
  evalState
    (makeUpdates [moveThings] mobs actions)
    (makeMovementGraph mobs actions)

aThing :: Pos -> Thing Spider Identity
aThing p = Thing (pure 't') (pure p) (pure $ Health 10) never

main :: IO ()
main =
  hspec $ do
    describe "move resolution" $ do
      it "4-cycle" $ do
        let
          mobs =
            Map.fromList
            [ (TPlayer, aThing $ Pos 0 0)
            , (TThing 0, aThing $ Pos 1 0)
            , (TThing 1, aThing $ Pos 1 1)
            , (TThing 2, aThing $ Pos 0 1)
            ]
          {-

          P(0,0) -->  T0(1,0)
            ^           |
            |           v
           T2(0,1) <--  T1(1,1)

          -}
          actions =
            Map.fromList
            [ (TPlayer, Move $ Relative R)
            , (TThing 0, Move $ Relative D)
            , (TThing 1, Move $ Relative L)
            , (TThing 2, Move $ Relative U)
            ]

          graph :: AdjacencyMap ThingType
          graph = buildGraph mobs actions

        Graph.scc graph `shouldBe`
          Graph.vertex
          (NonEmpty.edges1
           [ (TPlayer, TThing 2)
           , (TThing 0, TPlayer)
           , (TThing 1, TThing 0)
           , (TThing 2, TThing 1)
           ])

        runMoves mobs actions `shouldBe`
          Map.fromList
          [ (TPlayer, mempty & _updatePos ?~ Pos 1 0)
          , (TThing 0, mempty & _updatePos ?~ Pos 1 1)
          , (TThing 1, mempty & _updatePos ?~ Pos 0 1)
          , (TThing 2, mempty & _updatePos ?~ Pos 0 0)
          ]

      it "4-cycle where one doesn't move" $ do
        let
          mobs =
            Map.fromList
            [ (TPlayer, aThing $ Pos 0 0)
            , (TThing 0, aThing $ Pos 1 0)
            , (TThing 1, aThing $ Pos 1 1)
            , (TThing 2, aThing $ Pos 0 1)
            ]
          {-

          P(0,0)      T0(1,0)
            ^           |
            |           v
           T2(0,1) <--  T1(1,1)

          -}
          actions =
            Map.fromList
            [ (TPlayer, Wait)
            , (TThing 0, Move $ Relative D)
            , (TThing 1, Move $ Relative L)
            , (TThing 2, Move $ Relative U)
            ]

          graph :: AdjacencyMap ThingType
          graph = buildGraph mobs actions

        Graph.scc graph `shouldBe`
          Graph.edges
          [ ( NonEmpty.vertex $ TPlayer
            , NonEmpty.vertex $ TThing 2
            )
          , ( NonEmpty.vertex $ TThing 1
            , NonEmpty.vertex $ TThing 0
            )
          , ( NonEmpty.vertex $ TThing 2
            , NonEmpty.vertex $ TThing 1
            )
          ]

        runMoves mobs actions  `shouldBe` mempty

      it "moving to the same place D DL" $ do
        let
          mobs =
            Map.fromList
            [ (TPlayer, aThing $ Pos 0 0)
            , (TThing 0, aThing $ Pos 1 0)
            ]
          actions =
            Map.fromList
            [ (TPlayer, Move $ Relative D)
            , (TThing 0, Move $ Relative DL)
            ]

          graph :: AdjacencyMap ThingType
          graph = buildGraph mobs actions

        Graph.scc graph `shouldBe`
          Graph.vertices
          [ NonEmpty.vertex $ TPlayer
          , NonEmpty.vertex $ TThing 0
          ]

        runMoves mobs actions `shouldBe` Map.singleton TPlayer (mempty & _updatePos ?~ Pos 0 1)
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

    let
      mobs =
        Map.fromList
        [ (TPlayer, aThing p1)
        , (TThing 0, aThing p2)
        ]
      {-

      P(x,y)--> <--T(x+2,y)

      -}
      actions =
        Map.fromList
        [ (TPlayer, Move $ Relative d1)
        , (TThing 0, Move $ Relative d2)
        ]

      graph :: AdjacencyMap ThingType
      graph = buildGraph mobs actions

    Graph.scc graph ===
      Graph.vertices
      [ NonEmpty.vertex $ TPlayer
      , NonEmpty.vertex $ TThing 0
      ]

    runMoves mobs actions === Map.singleton TPlayer (mempty & _updatePos ?~ p)
