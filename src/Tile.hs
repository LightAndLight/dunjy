{-# language RankNTypes #-}
module Tile where

import Reflex.Class (Reflex)
import Reflex.Dynamic (Dynamic)
import Data.Functor.Identity (Identity(..))
import Data.Map (Map)
import Data.Set (Set)

import qualified Data.Map as Map
import qualified Data.Set as Set

import Pos

data Tile t f
  = Tile
  { _tileOccupants :: f (Set Int)
  }

distTileD ::
  Reflex t =>
  Tile t (Dynamic t) ->
  Dynamic t (Tile t Identity)
distTileD (Tile d) = Tile . Identity <$> d

newTileAt ::
  Functor f =>
  f (Map Int (Pos, a)) -> -- ^ positions of things
  Pos ->
  Tile t f
newTileAt dThings pos =
  Tile
  { _tileOccupants =
      Map.foldrWithKey
        (\k (p, _) b -> if p == pos then Set.insert k b else b)
        Set.empty <$>
      dThings
  }
