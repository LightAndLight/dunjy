module Level where

import Data.Vector (Vector)

import qualified Data.Vector as Vector

import Pos
import Tile

data Level t occ
  = Level
  { _levelWidth :: Int
  , _levelHeight :: Int
  , _levelData :: Vector (Tile t occ)
  }

newLevel ::
  Monad m =>
  Int ->
  Int ->
  (Int -> Int -> m (Tile t occ)) ->
  m (Level t occ)
newLevel w h mk = Level w h <$> Vector.unfoldrNM (w*h) make (0, 0)
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
levelPos (Pos x y) (Level w h ts)
  | x < 0 || x >= w || y < 0 || y >= h = Nothing
  | otherwise = Just $ Vector.unsafeIndex ts (y * w + x)
