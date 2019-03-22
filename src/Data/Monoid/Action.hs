{-# language FunctionalDependencies, MultiParamTypeClasses #-}
module Data.Monoid.Action where

import Data.Foldable (foldl')

-- |
-- @act mempty = id@
-- @act (a <> b) = act a . act b@
class Monoid m => MonoidAction m s | m -> s where
  act :: m -> s -> s

-- | @actsr ms z = foldr act z ms@
actsr :: (Foldable f, MonoidAction m s) => f m -> s -> s
actsr ms z = foldr act z ms

-- | @actsl ms z = foldl' (flip act) z ms@
actsl :: (Foldable f, MonoidAction m s) => f m -> s -> s
actsl ms z = foldl' (flip act) z ms
