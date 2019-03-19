{-# language BangPatterns #-}
{-# language RankNTypes #-}
{-# language ScopedTypeVariables #-}
module Level where

import Reflex.Class (Reflex, coerceDynamic)
import Reflex.Dynamic (Dynamic, distributeMapOverDynPure)

import Data.Functor.Identity (Identity)
import Data.Map (Map)
import Data.Vector (Vector)

import qualified Data.Map as Map

import Pos
import Tile

newtype Tiles f occ
  = Tiles
  { unTiles :: Map Int (Tile f occ)
  }

distTiles ::
  Applicative m =>
  (forall x. f x -> m (g x)) ->
  Tiles f occ ->
  m (Tiles g occ)
distTiles fun = fmap Tiles . traverse (distTile fun) . unTiles

distTilesI ::
  Applicative f =>
  Tiles f occ ->
  f (Tiles Identity occ)
distTilesI = distTiles (fmap pure)

distTilesD ::
  Reflex t =>
  Tiles (Dynamic t) occ ->
  Dynamic t (Tiles Identity occ)
distTilesD (Tiles ts) =
  coerceDynamic (distributeMapOverDynPure $ fmap distTileI ts)

data Level f occ
  = Level
  { _levelWidth :: Int
  , _levelHeight :: Int
  , _levelData :: Tiles f occ
  }

distLevel ::
  Applicative m =>
  (forall x. f x -> m (g x)) ->
  Level f occ ->
  m (Level g occ)
distLevel fun (Level w h ts)= Level w h <$> distTiles fun ts

distLevelI ::
  Applicative f =>
  Level f occ ->
  f (Level Identity occ)
distLevelI = distLevel (fmap pure)

distLevelD ::
  Reflex t =>
  Level (Dynamic t) occ ->
  Dynamic t (Level Identity occ)
distLevelD (Level w h ts) = Level w h <$> distTilesD ts

newLevel ::
  forall m f occ.
  Monad m =>
  Int ->
  Int ->
  (Int -> Int -> m (Tile f occ)) ->
  m (Level f occ)
newLevel w h mk = Level w h . Tiles <$> make 0 0 0
  where
    make :: Int -> Int -> Int -> m (Map Int (Tile f occ))
    make !n !x !y =
      if x == w
      then
        if y == h
        then pure mempty
        else make n 0 (y+1)
      else
        Map.insert n <$> mk x y <*> make (n+1) (x+1) y

levelPos :: Pos -> Level t occ -> Maybe (Tile t occ)
levelPos (Pos x y) (Level w h (Tiles ts))
  | x < 0 || x >= w || y < 0 || y >= h = Nothing
  | otherwise = Map.lookup (y * w + x) ts
