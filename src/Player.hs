{-# language FlexibleContexts #-}
{-# language ScopedTypeVariables #-}
{-# language TemplateHaskell #-}
module Player where

import Reflex.Class (Reflex, Behavior, Event, MonadHold, leftmost, (<@))
import Reflex.Dynamic (Dynamic, current)

import Control.Lens.Getter ((^.))
import Control.Lens.TH (makeLenses)
import Control.Monad.Fix (MonadFix)
import Data.Map (Map)
import Data.Maybe (fromMaybe)
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
  , Pos -> Health -> Event t (Updates t) -> m (Thing t (Dynamic t))
  )
initPlayer pc dMobPositions =
  ( () <$ eTick
  , \pos health eUpdates -> mkThing pos health (pure '@') eAction eUpdates
  )
  where
    bAdjacentMobs :: Behavior t (Set Dir)
    bAdjacentMobs =
      current $
      (\mobs ->
         fromMaybe Set.empty $ do
           ppos <- Map.lookup TPlayer mobs
           pure $
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
      dMobPositions

    moveOrAttack d l =
      (\m -> if m then Melee d else Move (Relative d)) <$>
      fmap (Set.member d) bAdjacentMobs <@
      (pc ^. l)

    moveOrAttackDirs =
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
      (Wait <$ (pc ^. pcWait)) :
      fmap (uncurry moveOrAttack) moveOrAttackDirs

    eAction = eTick
