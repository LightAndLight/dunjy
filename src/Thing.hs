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
  ( Reflex, Event, MonadHold, EventSelector, coerceEvent, fan, select, coerceDynamic
  , alignEventWithMaybe
  )
import Reflex.Dynamic (Dynamic, holdDyn, foldDyn)

import Control.Lens.Getter ((^.))
import Control.Lens.Lens (Lens', lens)
import Control.Lens.Setter ((%~), (<>~))
import Control.Lens.TH (makeLenses)
import Control.Lens.Wrapped (_Wrapped)
import Control.Monad.Fix (MonadFix)
import Data.Coerce (coerce)
import Data.Dependent.Map (DMap)
import Data.Dependent.Sum (ShowTag(..))
import Data.Foldable (fold)
import Data.GADT.Compare.TH (deriveGEq, deriveGCompare)
import Data.GADT.Show.TH (deriveGShow)
import Data.Maybe (fromMaybe)
import Data.Monoid (Sum(..))
import Data.Function ((&))
import Data.Functor.Identity (Identity(..))
import Data.These (These(..))

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
  UpdateHealing :: Update Health
  UpdateDamage :: Update Damage
deriveGEq ''Update
deriveGCompare ''Update
deriveGShow ''Update

newtype Updates t = Updates { unUpdates :: DMap Update Identity }
  deriving Show
instance ShowTag Update Identity where
  showTaggedPrec UpdateHealing = showsPrec
  showTaggedPrec UpdatePos = showsPrec
  showTaggedPrec UpdateDamage = showsPrec
class UpdateDamage s where; _updateDamage :: Lens' s (Maybe Damage)
class UpdateHealing s where; _updateHealing :: Lens' s (Maybe Health)
class UpdatePos s where; _updatePos :: Lens' s (Maybe Pos)
instance Semigroup (Updates t) where
  Updates a <> Updates b =
    Updates $
    DMap.unionWithKey
    (\k x y ->
       case k of
         UpdateDamage -> x <> y
         UpdateHealing -> x <> y
         UpdatePos -> y)
    a
    b
instance Monoid (Updates t) where
  mempty = Updates mempty

fanUpdates :: Reflex t => Event t (Updates t) -> EventSelector t Update
fanUpdates = fan . coerceEvent

data Thing t f
  = Thing
  { _thingSprite :: f Char
  , _thingPos :: f Pos
  , _thingHealth :: f Health
  , _thingAction :: Event t Action
  }

sequenceThing :: Reflex t => Thing t (Dynamic t) -> Dynamic t (Thing t Identity)
sequenceThing (Thing a b c d) =
  Thing <$>
  coerceDynamic a <*>
  coerceDynamic b <*>
  coerceDynamic c <*>
  pure d
class HasHealth f s | s -> f where; _health :: Lens' s (f Health)
class HasPos f s | s -> f where; _pos :: Lens' s (f Pos)

mkThing ::
  (Reflex t, MonadHold t m, MonadFix m) =>
  Pos -> -- ^ initial position
  Health -> -- ^ initial health
  Dynamic t Char -> -- ^ sprite
  Event t Action -> -- ^ its action
  Event t (Updates t) -> -- ^ update event
  m (Thing t (Dynamic t))
mkThing pos health dSprite eAction eUpdate = do
  let updates = fanUpdates eUpdate
  dPos <- holdDyn pos $ select updates UpdatePos
  dHealth <-
    foldDyn (<>) health $
    alignEventWithMaybe
      (\case
          This a -> Just . act a $ Health 0
          That a -> Just a
          These a b -> Just $ act a b)
      (select updates UpdateDamage)
      (select updates UpdateHealing)
  pure $
    Thing
    { _thingSprite = dSprite
    , _thingPos = dPos
    , _thingHealth = dHealth
    , _thingAction = eAction
    }

makeLenses ''Thing

mkUpdateLens :: Update a -> Lens' (Updates t) (Maybe a)
mkUpdateLens k =
  lens
    (coerce . DMap.lookup k . unUpdates)
    (\(Updates u) v ->
        Updates $
        maybe
          (DMap.delete k u)
          (\v' -> DMap.insert k (Identity v') u)
          v)

instance UpdateDamage (Updates t) where; _updateDamage = mkUpdateLens UpdateDamage
instance UpdateHealing (Updates t) where; _updateHealing = mkUpdateLens UpdateHealing
instance UpdatePos (Updates t) where; _updatePos = mkUpdateLens UpdatePos
instance HasHealth f (Thing t f) where; _health = thingHealth
instance HasPos f (Thing t f) where; _pos = thingPos

instance MonoidAction (Updates t) (Thing t Identity) where
  act m t =
    t &
    thingPos._Wrapped %~
      flip fromMaybe (m ^. _updatePos) &
    thingHealth._Wrapped <>~
      act (fold $ m ^. _updateDamage) (fold $ m ^. _updateHealing)
