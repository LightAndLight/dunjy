{-# language DerivingVia #-}
{-# language FlexibleContexts #-}
{-# language FlexibleInstances #-}
{-# language GADTs #-}
{-# language LambdaCase #-}
{-# language MultiParamTypeClasses #-}
{-# language RankNTypes #-}
{-# language RecursiveDo #-}
{-# language ScopedTypeVariables #-}
{-# language TemplateHaskell #-}
module Thing where

import Reflex.Class (Reflex, Event, MonadHold)
import Reflex.Dynamic (Dynamic)

import Control.Monad.Fix (MonadFix)
import Data.Dependent.Map (DMap)
import Data.Maybe (fromMaybe)
import Data.Monoid (Last(..), Sum(..))
import Data.Function ((&))
import Data.Functor.Identity (Identity(..))
import Lens.Micro (Lens', (%~))
import Lens.Micro.TH (makeLenses)

import Data.Monoid.Action
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

newtype Health = Health { unHealth :: Int }
  deriving (Eq, Ord, Show)
  deriving (Semigroup, Monoid) via (Sum Int)

instance MonoidAction Health Health where; act = (<>)

newtype Damage = Damage { unDamage :: Int }
  deriving (Eq, Ord, Show)
  deriving (Semigroup, Monoid) via (Sum Int)

instance MonoidAction Damage Health where
  act (Damage n) (Health h) = Health (h - n)

data UpdateThing t
  = UpdateThing
  { _updateThingPos :: Maybe Pos
  , _updateThingHealth :: Maybe Health
  } deriving (Eq, Show, Ord)
class UpdateHealth s where; updateHealth_ :: Lens' s (Maybe Health)
class UpdatePos s where; updatePos_ :: Lens' s (Maybe Pos)
instance Semigroup (UpdateThing t) where
  UpdateThing a b <> UpdateThing a' b' =
    UpdateThing
      (getLast $ Last a <> Last a')
      (getLast $ Last b <> Last b')
instance Monoid (UpdateThing t) where
  mempty = UpdateThing (getLast mempty) (getLast mempty)

data Thing t
  = Thing
  { _thingSprite :: Dynamic t Char
  , _thingPos :: Pos
  , _thingHealth :: Health
  , _thingAction :: Event t (DMap Action Identity)
  }
class HasHealth s where; health_ :: Lens' s Health
class HasPos s where; pos_ :: Lens' s Pos

mkThing ::
  (Reflex t, MonadHold t m, MonadFix m) =>
  Pos -> -- ^ initial position
  Health -> -- ^ initial health
  Dynamic t Char -> -- ^ sprite
  Event t (DMap Action Identity) -> -- ^ its actions
  m (Thing t)
mkThing pos health dSprite eAction = do
  pure $
    Thing
    { _thingSprite = dSprite
    , _thingPos = pos
    , _thingHealth = health
    , _thingAction = eAction
    }

makeLenses ''Thing
makeLenses ''UpdateThing

instance MonoidAction (UpdateThing t) (Thing t) where
  act (UpdateThing mp mh) t =
    t &
    thingPos %~ flip fromMaybe mp &
    thingHealth %~ flip fromMaybe mh

instance UpdateHealth (UpdateThing t) where; updateHealth_ = updateThingHealth
instance UpdatePos (UpdateThing t) where; updatePos_ = updateThingPos
instance HasHealth (Thing t) where; health_ = thingHealth
instance HasPos (Thing t) where; pos_ = thingPos
