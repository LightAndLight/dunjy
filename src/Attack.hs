{-# language FlexibleContexts #-}
module Attack where

import Control.Lens.Fold ((^?))
import Control.Lens.Getter ((^.), view)
import Control.Lens.Setter ((?~))
import Control.Lens.Wrapped (_Wrapped)
import Control.Monad (guard)
import Data.Function ((&))
import Data.Functor.Identity (Identity(..))
import Data.Map (Map)

import qualified Data.Map as Map

import Data.Monoid.Action (act)
import Action
import Thing
import ThingType

runMelees ::
  ( HasPos Identity a, HasHealth Identity a
  , AsMelee b
  , Monoid c, UpdateDamage c
  ) =>
  Map ThingType a -> -- ^ all the things
  Map ThingType b -> -- ^ things attacking
  Map ThingType c -- ^ things receiving damage
runMelees things =
  Map.foldlWithKey
    (\rest k action ->
       maybe
         rest
         (\(kHealth, target) ->
            case Map.lookup k rest >>= view _updateDamage of
              -- this thing was killed this turn
              Just dmg | act dmg kHealth <= mempty -> rest
              -- this thing didn't die this turn
              _ -> Map.insertWith (<>) target (mempty & _updateDamage ?~ Damage 2) rest)
         (do
            dir <- action ^? _Melee
            res <- Map.lookup k things
            target <- findAtPos (runMove' (res ^. _pos._Wrapped) $ Relative dir) things
            pure (res ^. _health._Wrapped, target)))
    mempty
  where
    findAtPos p =
      Map.foldrWithKey
        (\k thing rest -> if p == thing ^. _pos._Wrapped then Just k else rest)
        Nothing

runMelees' ::
  ( HasPos Identity a, HasHealth Identity a
  , AsMelee b
  ) =>
  Map ThingType a -> -- ^ mobs, to be accessed during the fold
  ThingType -> -- ^ mob id
  b -> -- ^ mob action
  Maybe (ThingType, Damage) -- ^ damage dealt, and to whom
runMelees' mobs tt action = do
  dir <- action ^? _Melee
  mob <- Map.lookup tt mobs
  target <- findAtPos (runMove' (mob ^. _pos._Wrapped) $ Relative dir) mobs
  guard $ (mob ^. _health._Wrapped) > Health 0
  pure (target, Damage 2)
  where
    findAtPos p =
      Map.foldrWithKey
        (\k thing rest -> if p == thing ^. _pos._Wrapped then Just k else rest)
        Nothing
