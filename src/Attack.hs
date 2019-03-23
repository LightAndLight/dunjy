module Attack where

import Control.Lens.Fold ((^?))
import Data.Map (Map)

import qualified Data.Map as Map

import Data.Monoid.Action (act)
import Action
import Pos
import Thing
import ThingType

runMelees ::
  AsMelee a =>
  Map ThingType (Pos, Health) -> -- ^ all the things
  Map ThingType a -> -- ^ things attacking
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
            (pos, health) <- Map.lookup k things
            target <- findAtPos (runMove' pos $ Relative dir) things
            pure (health, target)))
    mempty
  where
    findAtPos p =
      Map.foldrWithKey
        (\k (pos, _) rest -> if p == pos then Just k else rest)
        Nothing
