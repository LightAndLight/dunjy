{-# language RankNTypes #-}
module Tile where

import Pos

data Tile t = Tile

newTileAt :: Pos -> Tile t
newTileAt _ = Tile
