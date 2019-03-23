{-# language FlexibleContexts #-}
module Attack where

import Control.Lens.Fold ((^?))
import Control.Lens.Getter ((^.))
import Control.Lens.Wrapped (_Wrapped)
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
  ) =>
  Map ThingType a -> -- ^ all the things
  Map ThingType b -> -- ^ things attacking
  Map ThingType Damage -- ^ things receiving damage
runMelees things =
  Map.foldlWithKey
    (\rest k action ->
       maybe
         rest
         (\(kHealth, target) ->
            case Map.lookup k rest of
              -- this thing was killed this turn
              Just dmg | act dmg kHealth <= mempty -> rest
              -- this thing didn't die this turn
              _ -> Map.insertWith (<>) target (Damage 2) rest)
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
