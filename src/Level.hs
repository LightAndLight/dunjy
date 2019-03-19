{-# language RankNTypes #-}
module Level where

import Data.Functor.Identity (Identity)
import Data.Vector (Vector)

import qualified Data.Vector as Vector

import Pos
import Tile

newtype Tiles f occ
  = Tiles
  { unTiles :: Vector (Tile f occ)
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

newLevel ::
  Monad m =>
  Int ->
  Int ->
  (Int -> Int -> m (Tile f occ)) ->
  m (Level f occ)
newLevel w h mk = Level w h . Tiles <$> Vector.unfoldrNM (w*h) make (0, 0)
  where
    make (x, y) =
      if x == w
      then
        if y == h
        then pure Nothing
        else make (0, y+1)
      else
        fmap Just $ (,) <$> mk x y <*> pure (x+1, y)

levelPos :: Pos -> Level t occ -> Maybe (Tile t occ)
levelPos (Pos x y) (Level w h (Tiles ts))
  | x < 0 || x >= w || y < 0 || y >= h = Nothing
  | otherwise = Just $ Vector.unsafeIndex ts (y * w + x)
