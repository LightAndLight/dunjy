{-# language BangPatterns #-}
{-# language RankNTypes #-}
{-# language ScopedTypeVariables #-}
module Level where

import Data.Map (Map)

import qualified Data.Map as Map

import Pos
import Tile

newtype Tiles t
  = Tiles
  { unTiles :: Map Int (Tile t)
  }

data Level t
  = Level
  { _levelWidth :: Int
  , _levelHeight :: Int
  , _levelData :: Tiles t
  }

newLevel ::
  forall t m.
  Monad m =>
  Int ->
  Int ->
  (Int -> Int -> m (Tile t)) ->
  m (Level t)
newLevel w h mk = Level w h . Tiles <$> make 0 0 0
  where
    make :: Int -> Int -> Int -> m (Map Int (Tile t))
    make !n !x !y =
      if x == w
      then
        if y == h
        then pure mempty
        else make n 0 (y+1)
      else
        Map.insert n <$> mk x y <*> make (n+1) (x+1) y

levelPos :: Pos -> Level t -> Maybe (Tile t)
levelPos (Pos x y) (Level w h (Tiles ts))
  | x < 0 || x >= w || y < 0 || y >= h = Nothing
  | otherwise = Map.lookup (y * w + x) ts
