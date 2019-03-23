{-# language FlexibleContexts #-}
{-# language ScopedTypeVariables #-}
{-# language TemplateHaskell #-}
module Player where

import Reflex.Class (Reflex, Behavior, Event, MonadHold, leftmost, gate)
import Reflex.Dynamic (Dynamic, current)

import Control.Lens.Getter ((^.))
import Control.Lens.TH (makeLenses)
import Control.Monad.Fix (MonadFix)
import Data.Map (Map)
import Data.Set (Set)

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

initPlayer ::
  forall t m.
  ( Reflex t, MonadHold t m, MonadFix m
  ) =>
  PlayerControls t -> -- ^ controls
  Dynamic t (Map ThingType Pos) -> -- ^ mob positions
  ( Event t ()
  , Pos -> Health -> Event t Updates -> m (Thing t (Dynamic t))
  )
initPlayer pc dMobPositions =
  ( () <$ eTick
  , \pos health eUpdates -> mkThing pos health (pure '@') eAction eUpdates
  )
  where
    dPlayerPos :: Dynamic t (Maybe Pos)
    dPlayerPos = Map.lookup TPlayer <$> dMobPositions

    bAdjacentMobs :: Behavior t (Set Dir)
    bAdjacentMobs =
      current $
      (\mppos mobs ->
         case mppos of
           Nothing -> Set.empty
           Just ppos ->
             foldr
               (\tpos ->
                  case subtractPos ppos tpos of
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
      dMobPositions

    attackDir d l = d <$ gate (Set.member d <$> bAdjacentMobs) (pc ^. l)
    moveDir d l = Move (Relative d) <$ gate (Set.notMember d <$> bAdjacentMobs) (pc ^. l)

    moveDirs =
      [ (L, pcLeft)
      , (UL, pcUpLeft)
      , (U, pcUp)
      , (UR, pcUpRight)
      , (R, pcRight)
      , (DR, pcDownRight)
      , (D, pcDown)
      , (DL, pcDownLeft)
      ]

    eTick :: Event t Action
    eTick =
      leftmost $
      fmap (uncurry moveDir) moveDirs <>
      [ Wait <$ (pc ^. pcWait)
      , Melee <$> leftmost (uncurry attackDir <$> moveDirs)
      ]

    eAction = eTick
