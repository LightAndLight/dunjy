{-# language FlexibleContexts #-}
{-# language ScopedTypeVariables #-}
{-# language TemplateHaskell #-}
module Player where

import Reflex.Class (Reflex, Behavior, Event, MonadHold, merge, leftmost, gate)
import Reflex.Dynamic (Dynamic, current)

import Control.Monad.Fix (MonadFix)
import Data.Dependent.Map (DMap)
import Data.Dependent.Sum (DSum(..))
import Data.Functor.Identity (Identity)
import Data.Map (Map)
import Data.Set (Set)
import Lens.Micro ((^.))
import Lens.Micro.TH (makeLenses)

import qualified Data.Dependent.Map as DMap
import qualified Data.Map as Map
import qualified Data.Set as Set

import Action
import Pos
import Thing
import ThingType

data PlayerControls t
  = PlayerControls
  { _pcLeft :: Event t ()
  , _pcUpLeft :: Event t ()
  , _pcUp :: Event t ()
  , _pcUpRight :: Event t ()
  , _pcRight :: Event t ()
  , _pcDownRight :: Event t ()
  , _pcDown :: Event t ()
  , _pcDownLeft :: Event t ()
  , _pcWait :: Event t ()
  }
makeLenses ''PlayerControls

mkPlayer ::
  forall t m.
  ( Reflex t, MonadHold t m, MonadFix m
  ) =>
  PlayerControls t -> -- ^ controls
  Pos -> -- ^ initial position
  Dynamic t (Map ThingType (Thing t)) -> -- ^ mobs
  Event t Int -> -- ^ received damage
  m (Event t (), Thing t)
mkPlayer pc pos dMobs eDamage = do
  res <- mkThing pos 10 (pure '@') eDamage eAction
  pure (() <$ eTick, res)
  where
    dPlayerPos :: Dynamic t (Maybe Pos)
    dPlayerPos = fmap _thingPos . Map.lookup TPlayer <$> dMobs

    bAdjacentMobs :: Behavior t (Set Dir)
    bAdjacentMobs =
      current $
      (\mppos mobs ->
         case mppos of
           Nothing -> Set.empty
           Just ppos ->
             foldr
               (\t ->
                  case subtractPos (t ^. thingPos) ppos of
                    Pos (-1) 0 -> Set.insert L
                    Pos (-1) (-1) -> Set.insert UL
                    Pos 0 (-1) -> Set.insert U
                    Pos 1 (-1) -> Set.insert UR
                    Pos 1 0 -> Set.insert R
                    Pos 1 1 -> Set.insert DR
                    Pos 0 1 -> Set.insert D
                    Pos (-1) 1 -> Set.insert DL
                    _ -> id)
               mempty
               mobs) <$>
      dPlayerPos <*>
      dMobs

    attackDir d e = d <$ gate (Set.member d <$> bAdjacentMobs) e

    attackDirs =
      [ (L, pcLeft)
      , (UL, pcUpLeft)
      , (U, pcUp)
      , (UR, pcUpRight)
      , (R, pcRight)
      , (DR, pcDownRight)
      , (D, pcDown)
      , (DL, pcDownLeft)
      ]

    eTick :: Event t (DMap Action Identity)
    eTick =
      merge . DMap.fromList $
      [ Move L  :=> () <$ (pc ^. pcLeft)
      , Move UL :=> () <$ (pc ^. pcUpLeft)
      , Move U  :=> () <$ (pc ^. pcUp)
      , Move UR :=> () <$ (pc ^. pcUpRight)
      , Move R  :=> () <$ (pc ^. pcRight)
      , Move DR :=> () <$ (pc ^. pcDownRight)
      , Move D  :=> () <$ (pc ^. pcDown)
      , Move DL :=> () <$ (pc ^. pcDownLeft)
      , Wait :=> (pc ^. pcWait)
      , Melee :=>
        leftmost ((\(d, l) -> attackDir d (pc ^. l)) <$> attackDirs)
      ]

    eAction = eTick
