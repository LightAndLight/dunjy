{-# language FlexibleContexts #-}
{-# language GADTs #-}
{-# language LambdaCase #-}
{-# language RankNTypes #-}
{-# language RecursiveDo #-}
{-# language ScopedTypeVariables #-}
{-# language TemplateHaskell #-}
module Thing where

import Reflex.Class (Reflex, Event, MonadHold, fmapMaybe)
import Reflex.Dynamic (Dynamic, holdDyn, foldDyn, updated)

import Control.Monad.Fix (MonadFix)
import Data.Dependent.Map (DMap)
import Data.Function ((&))
import Data.Functor.Identity (Identity(..))
import Lens.Micro ((%~))
import Lens.Micro.TH (makeLenses)

import Action
import Pos

data Status
  = Alive
  | Dead
  deriving (Eq, Show, Ord)

runMove :: Dir -> Int -> Pos -> Pos
runMove dir dist pos =
  case dir of
    L -> pos & posX %~ subtract dist
    UL -> pos & posX %~ subtract dist & posY %~ subtract dist
    U -> pos & posY %~ subtract dist
    UR -> pos & posY %~ subtract dist & posX %~ (+ dist)
    R -> pos & posX %~ (+ dist)
    DR -> pos & posX %~ (+ dist) & posY %~ (+ dist)
    D -> pos & posY %~ (+ dist)
    DL -> pos & posY %~ (+ dist) & posX %~ subtract dist

runMove' :: Pos -> Move -> Pos
runMove' pos (Relative dir) = runMove dir 1 pos
runMove' _ (Absolute pos) = pos

data Thing t
  = Thing
  { _thingSprite :: Dynamic t Char
  , _thingHealth :: Dynamic t Int
  , _thingStatus :: Dynamic t Status
  , _thingAction :: Event t (DMap Action Identity)
  }

mkThing ::
  (Reflex t, MonadHold t m, MonadFix m) =>
  Int -> -- ^ initial health
  Dynamic t Char -> -- ^ sprite
  Event t Int -> -- ^ damage
  Event t (DMap Action Identity) -> -- ^ its actions
  m (Thing t)
mkThing health dSprite eDamage eAction = do
  dHealth <- foldDyn subtract health eDamage
  dStatus <-
    holdDyn Alive $
    fmapMaybe
      (\h -> if h <= 0 then Just Dead else Nothing)
      (updated dHealth)
  pure $
    Thing
    { _thingSprite = dSprite
    , _thingHealth = dHealth
    , _thingStatus = dStatus
    , _thingAction = eAction
    }

makeLenses ''Thing
