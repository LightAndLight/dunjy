{-# language FunctionalDependencies, MultiParamTypeClasses #-}
{-# language UndecidableInstances #-}
module Data.Monoid.Action where

import Data.Foldable (foldl')
import Data.Map (Map)
import Data.Map.Monoidal (MonoidalMap(..))

import qualified Data.Map as Map

-- |
-- @act mempty = id@
-- @act (a <> b) = act a . act b@
class Monoid m => MonoidAction m s | m -> s where
  act :: m -> s -> s

instance (Ord k, MonoidAction m s) => MonoidAction (MonoidalMap k m) (Map k s) where
  act (MonoidalMap m) = Map.mapWithKey (\k ss -> maybe ss (\mm -> act mm ss) (Map.lookup k m))

-- | @actsr ms z = foldr act z ms@
actsr :: (Foldable f, MonoidAction m s) => f m -> s -> s
actsr ms z = foldr act z ms

-- | @actsl ms z = foldl' (flip act) z ms@
actsl :: (Foldable f, MonoidAction m s) => f m -> s -> s
actsl ms z = foldl' (flip act) z ms
