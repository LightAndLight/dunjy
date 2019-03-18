{-# language FlexibleContexts #-}
{-# language GADTs #-}
{-# language RankNTypes #-}
{-# language RecursiveDo #-}
{-# language ScopedTypeVariables #-}
{-# language TemplateHaskell #-}
module Thing where

import Reflex.Class (Reflex, Event, MonadHold, fmapMaybe)
import Reflex.Dynamic (Dynamic, holdDyn, foldDyn, updated)

import Control.Monad (join)
import Control.Monad.Fix (MonadFix)
import Data.Function ((&))
import Data.GADT.Compare.TH (deriveGEq, deriveGCompare)
import Data.List.NonEmpty (NonEmpty)
import Lens.Micro ((%~))
import Lens.Micro.TH (makeLenses)

import Action
import Level
import Pos
import Tile

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

data Thing t
  = Thing
  { _thingSprite :: Dynamic t Char
  , _thingHealth :: Dynamic t Int
  , _thingStatus :: Dynamic t Status
  , _thingAction :: Event t (NonEmpty Action)
  }

mkPos ::
  forall t m.
  (Reflex t, MonadHold t m, MonadFix m) =>
  Level t (Thing t) ->
  Event t (NonEmpty Action) ->
  Pos ->
  m (Dynamic t Pos)
mkPos level eAction initialPos =
  join <$> foldDyn goPos (pure initialPos) eAction
  where
    goPos :: NonEmpty Action -> Dynamic t Pos -> Dynamic t Pos
    goPos acts p =
      foldr
        (\a b ->
           case a of
             Move dir dist -> do
               p' <- runMove dir dist <$> b
               case levelPos p' level of
                 Nothing -> b
                 Just tile ->
                   (\occs b' -> if null occs then p' else b') <$>
                   _tileOccupants tile <*>
                   b
             MoveTo pos -> pure pos
             _ -> b)
        p
        acts

data KThing a where
  KPlayer :: KThing (NonEmpty Action)
  KThing :: Int -> KThing (NonEmpty Action)
deriveGEq ''KThing
deriveGCompare ''KThing

mkThing ::
  (Reflex t, MonadHold t m, MonadFix m) =>
  Int -> -- ^ initial health
  Dynamic t Char -> -- ^ sprite
  Event t Int -> -- ^ damage
  Event t (NonEmpty Action) ->
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
