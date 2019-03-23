module Attack where

import Data.Map (Map)

import qualified Data.Map as Map

import Data.Monoid.Action (act)
import Action
import Pos
import Thing
import ThingType

runMelees ::
  Map ThingType Pos -> -- ^ all the things
  Map ThingType (Health, Dir) -> -- ^ things attacking
  Map ThingType Damage -- ^ things receiving damage
runMelees thingLocs =
  Map.foldlWithKey
    (\rest k (health, dir) ->
       case Map.lookup k thingLocs of
         Nothing -> rest
         Just pos ->
           case findAtPos (runMove' pos $ Relative dir) thingLocs of
             Nothing -> rest
             Just target ->
               case Map.lookup k rest of
                 -- this thing was killed this turn
                 Just dmg | act dmg health <= mempty -> rest
                 -- this thing didn't die this turn
                 _ -> Map.insertWith (<>) target (Damage 2) rest)
    mempty
  where
    findAtPos p =
      Map.foldrWithKey
        (\k pos rest -> if p == pos then Just k else rest)
        Nothing
