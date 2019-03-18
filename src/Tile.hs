module Tile where

import Reflex.Class (Reflex, distributeListOverDyn, fmapMaybe)
import Reflex.Dynamic (Dynamic)

import Pos

data Tile t occ
  = Tile
  { _tileOccupants :: Dynamic t [occ]
  }

newTileAt :: Reflex t => Dynamic t [Positioned t a] -> Pos -> Tile t a
newTileAt dThings pos =
  Tile
  { _tileOccupants = do
      dThings >>= \things ->
        -- Dynamic [(Thing, Pos)]
        -- filter to only the things that are here
        fmap (fmapMaybe (\(t, p) -> if p == pos then Just t else Nothing)) .
        -- distribute
        distributeListOverDyn $
        -- tag each position with the thing
        (\(Positioned thing dPos) -> (,) thing <$> dPos) <$> things
  }
