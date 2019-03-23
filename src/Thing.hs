{-# options_ghc -fno-warn-unused-matches #-}
{-# language DerivingVia #-}
{-# language FlexibleContexts #-}
{-# language FlexibleInstances #-}
{-# language FunctionalDependencies #-}
{-# language GADTs #-}
{-# language LambdaCase #-}
{-# language MultiParamTypeClasses #-}
{-# language RankNTypes #-}
{-# language RecursiveDo #-}
{-# language ScopedTypeVariables #-}
{-# language TemplateHaskell #-}
module Thing where

import Reflex.Class
  (Reflex, Event, MonadHold, EventSelector, coerceEvent, fan, select, coerceDynamic)
import Reflex.Dynamic (Dynamic, holdDyn)

import Control.Monad.Fix (MonadFix)
import Data.Coerce (coerce)
import Data.Dependent.Map (DMap)
import Data.Dependent.Sum (ShowTag(..))
import Data.GADT.Compare.TH (deriveGEq, deriveGCompare)
import Data.GADT.Show.TH (deriveGShow)
import Data.Monoid (Sum(..))
import Data.Function ((&))
import Data.Functor.Identity (Identity(..))
import Lens.Micro ((%~), Lens', lens)
import Lens.Micro.TH (makeLenses)

import qualified Data.Dependent.Map as DMap

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

data Update a where
  UpdatePos :: Update Pos
  UpdateHealth :: Update Health
deriveGEq ''Update
deriveGCompare ''Update
deriveGShow ''Update

newtype Updates = Updates { unUpdates :: DMap Update Identity }
  deriving Show
instance ShowTag Update Identity where
  showTaggedPrec UpdateHealth = showsPrec
  showTaggedPrec UpdatePos = showsPrec
class UpdateHealth s where; updateHealth_ :: Lens' s (Maybe Health)
class UpdatePos s where; updatePos_ :: Lens' s (Maybe Pos)
instance Semigroup Updates where
  Updates a <> Updates b = Updates $ DMap.union a b
instance Monoid Updates where
  mempty = Updates mempty

fanUpdates :: Reflex t => Event t Updates -> EventSelector t Update
fanUpdates = fan . coerceEvent

data Thing t f
  = Thing
  { _thingSprite :: f Char
  , _thingPos :: f Pos
  , _thingHealth :: f Health
  , _thingAction :: Event t (DMap Action Identity)
  }

sequenceThing :: Reflex t => Thing t (Dynamic t) -> Dynamic t (Thing t Identity)
sequenceThing (Thing a b c d) =
  Thing <$>
  coerceDynamic a <*>
  coerceDynamic b <*>
  coerceDynamic c <*>
  pure d
class HasHealth f s | s -> f where; health_ :: Lens' s (f Health)
class HasPos f s | s -> f where; pos_ :: Lens' s (f Pos)

mkThing ::
  (Reflex t, MonadHold t m, MonadFix m) =>
  Pos -> -- ^ initial position
  Health -> -- ^ initial health
  Dynamic t Char -> -- ^ sprite
  Event t (DMap Action Identity) -> -- ^ its actions
  Event t Updates -> -- ^ update event
  m (Thing t (Dynamic t))
mkThing pos health dSprite eAction eUpdate = do
  let updates = fanUpdates eUpdate
  dPos <- holdDyn pos $ select updates UpdatePos
  dHealth <- holdDyn health $ select updates UpdateHealth
  pure $
    Thing
    { _thingSprite = dSprite
    , _thingPos = dPos
    , _thingHealth = dHealth
    , _thingAction = eAction
    }

makeLenses ''Thing

mkUpdateLens :: Update a -> Lens' Updates (Maybe a)
mkUpdateLens k =
  lens
    (coerce . DMap.lookup k . unUpdates)
    (\(Updates u) v ->
        Updates $
        maybe
          (DMap.delete k u)
          (\v' -> DMap.insert k (Identity v') u)
          v)

instance UpdateHealth Updates where; updateHealth_ = mkUpdateLens UpdateHealth
instance UpdatePos Updates where; updatePos_ = mkUpdateLens UpdatePos
instance HasHealth f (Thing t f) where; health_ = thingHealth
instance HasPos f (Thing t f) where; pos_ = thingPos
