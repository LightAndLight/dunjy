{-# language RankNTypes #-}
module Tile where

import Reflex.Class (Reflex, fmapMaybe)
import Reflex.Dynamic (Dynamic, distributeMapOverDynPure)
import Data.Functor.Identity (Identity)
import Data.Map (Map)

import Pos

data Tile f occ
  = Tile
  { _tileOccupants :: f [occ]
  }

distTile ::
  Applicative m =>
  (forall x. f x -> m (g x)) ->
  Tile f occ ->
  m (Tile g occ)
distTile fun (Tile a) = Tile <$> fun a

distTileI :: Applicative f => Tile f occ -> f (Tile Identity occ)
distTileI = distTile (fmap pure)

newTileAt ::
  Reflex t =>
  Dynamic t (Map Int (Positioned t a)) ->
  Pos ->
  Tile (Dynamic t) a
newTileAt dThings pos =
  Tile
  { _tileOccupants = do
      dThings >>= \things ->
        -- Dynamic [(Thing, Pos)]
        -- filter to only the things that are here
        fmap (foldr (\(t, p) b -> if p == pos then t : b else b) []) .
        -- distribute
        distributeMapOverDynPure $
        -- tag each position with the thing
        (\(Positioned thing dPos) -> (,) thing <$> dPos) <$> things
  }
