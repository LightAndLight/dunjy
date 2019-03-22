module Attack where

import Data.Map (Map)
import Lens.Micro ((^.))

import qualified Data.Map as Map

import Data.Monoid.Action (act)
import Action
import Thing
import ThingType

runMelees ::
  ( HasPos a, HasHealth a
  , HasPos b
  ) =>
  Map ThingType b -> -- ^ all the things
  Map ThingType (a, Dir) -> -- ^ things attacking
  Map ThingType Damage -- ^ things receiving damage
runMelees thingLocs =
  Map.foldlWithKey
    (\rest k (thing, dir) ->
       case findAtPos (runMove' (thing ^. pos_) $ Relative dir) thingLocs of
         Nothing -> rest
         Just target ->
           case Map.lookup k rest of
             -- this thing was killed this turn
             Just dmg | act dmg (thing ^. health_) <= mempty -> rest
             -- this thing didn't die this turn
             _ -> Map.insertWith (<>) target (Damage 2) rest)
    mempty
  where
    findAtPos p =
      Map.foldrWithKey
        (\k thing rest -> if p == (thing ^. pos_) then Just k else rest)
        Nothing
