{-# language BangPatterns #-}
{-# language RankNTypes #-}
{-# language ScopedTypeVariables #-}
module Level where

import Reflex.Class (Reflex, coerceDynamic)
import Reflex.Dynamic (Dynamic, distributeMapOverDynPure)

import Data.Functor.Identity (Identity)
import Data.Map (Map)

import qualified Data.Map as Map

import Pos
import Tile

newtype Tiles t f
  = Tiles
  { unTiles :: Map Int (Tile t f)
  }

distTilesD ::
  Reflex t =>
  Tiles t (Dynamic t) ->
  Dynamic t (Tiles t Identity)
distTilesD (Tiles ts) =
  coerceDynamic (distributeMapOverDynPure $ distTileD <$> ts)

data Level t f
  = Level
  { _levelWidth :: Int
  , _levelHeight :: Int
  , _levelData :: Tiles t f
  }

distLevelD ::
  Reflex t =>
  Level t (Dynamic t) ->
  Dynamic t (Level t Identity)
distLevelD (Level w h ts) = Level w h <$> distTilesD ts

newLevel ::
  forall t m f.
  Monad m =>
  Int ->
  Int ->
  (Int -> Int -> m (Tile t f)) ->
  m (Level t f)
newLevel w h mk = Level w h . Tiles <$> make 0 0 0
  where
    make :: Int -> Int -> Int -> m (Map Int (Tile t f))
    make !n !x !y =
      if x == w
      then
        if y == h
        then pure mempty
        else make n 0 (y+1)
      else
        Map.insert n <$> mk x y <*> make (n+1) (x+1) y

levelPos :: Pos -> Level t f -> Maybe (Tile t f)
levelPos (Pos x y) (Level w h (Tiles ts))
  | x < 0 || x >= w || y < 0 || y >= h = Nothing
  | otherwise = Map.lookup (y * w + x) ts
