{-# language FlexibleContexts #-}
{-# language GADTs #-}
{-# language TemplateHaskell #-}
module Thing where

import Reflex.Class (Reflex, Event, MonadHold, fmapMaybe)
import Reflex.Dynamic (Dynamic, holdDyn, foldDyn, updated)

import Control.Monad.Fix (MonadFix)
import Data.Function ((&))
import Data.GADT.Compare.TH (deriveGEq, deriveGCompare)
import Data.List.NonEmpty (NonEmpty)
import Lens.Micro ((%~))
import Lens.Micro.TH (makeLenses)

import Action
import Pos

data Status
  = Alive
  | Dead
  deriving (Eq, Show, Ord)

data Thing t
  = Thing
  { _thingSprite :: Dynamic t Char
  , _thingPos :: Dynamic t Pos
  , _thingHealth :: Dynamic t Int
  , _thingStatus :: Dynamic t Status
  , _thingAction :: Event t (NonEmpty Action)
  }
makeLenses ''Thing

makePos ::
  (Reflex t, MonadHold t m, MonadFix m) =>
  Event t (NonEmpty Action) ->
  Pos ->
  m (Dynamic t Pos)
makePos eAction initialPos =
  foldDyn goPos initialPos eAction
  where
    goPos :: NonEmpty Action -> Pos -> Pos
    goPos acts p =
      foldr
        (\a b ->
           case a of
             Move dir dist ->
               case dir of
                 L -> b & posX %~ subtract dist
                 UL -> b & posX %~ subtract dist & posY %~ subtract dist
                 U -> b & posY %~ subtract dist
                 UR -> b & posY %~ subtract dist & posX %~ (+ dist)
                 R -> b & posX %~ (+ dist)
                 DR -> b & posX %~ (+ dist) & posY %~ (+ dist)
                 D -> b & posY %~ (+ dist)
                 DL -> b & posY %~ (+ dist) & posX %~ subtract dist
             MoveTo pos -> pos
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
  Pos -> -- ^ initial position
  Int -> -- ^ initial health
  Dynamic t Char -> -- ^ sprite
  Event t Int -> -- ^ damage
  Event t (NonEmpty Action) ->
  m (Thing t)
mkThing pos health dSprite eDamage eAction = do
  dPos <- makePos eAction pos
  dHealth <- foldDyn subtract health eDamage
  dStatus <-
    holdDyn Alive $
    fmapMaybe
      (\h -> if h <= 0 then Just Dead else Nothing)
      (updated dHealth)
  pure $
    Thing
    { _thingSprite = dSprite
    , _thingPos = dPos
    , _thingHealth = dHealth
    , _thingStatus = dStatus
    , _thingAction = eAction
    }
