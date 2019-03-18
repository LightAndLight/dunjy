{-# language FlexibleContexts #-}
{-# language GADTs #-}
{-# language RankNTypes #-}
{-# language RecursiveDo #-}
{-# language ScopedTypeVariables #-}
{-# language TemplateHaskell #-}
module Thing where

import Reflex.Class ((<@>), Reflex, Event, MonadHold, fmapMaybe, distributeListOverDyn, current)
import Reflex.Dynamic (Dynamic, holdDyn, foldDyn, updated)

import Control.Monad.Fix (MonadFix)
import Data.Function ((&))
import Data.Functor.Identity (Identity(..))
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

data Adjacent f t
  = Adjacent
  { _adjL :: f [Thing t]
  , _adjUL :: f [Thing t]
  , _adjU :: f [Thing t]
  , _adjUR :: f [Thing t]
  , _adjR :: f [Thing t]
  , _adjDR :: f [Thing t]
  , _adjD :: f [Thing t]
  , _adjDL :: f [Thing t]
  }

distAdjacent :: Applicative g => (forall x. f x -> g (h x)) -> Adjacent f t -> g (Adjacent h t)
distAdjacent fun (Adjacent a b c d e f g h) =
  Adjacent <$>
  fun a <*>
  fun b <*>
  fun c <*>
  fun d <*>
  fun e <*>
  fun f <*>
  fun g <*>
  fun h

distAdjacentI :: Applicative f => Adjacent f t -> f (Adjacent Identity t)
distAdjacentI = distAdjacent (fmap pure)

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

mkAdjacent :: forall t. Reflex t => Dynamic t Pos -> Dynamic t [Thing t] -> Adjacent (Dynamic t) t
mkAdjacent dPos things =
  Adjacent
  { _adjL = do
      pos <- dPos
      let posL = runMove L 1 pos
      fmapMaybe (\(t, p) -> if p == posL then Just t else Nothing) <$> thingsPos
  , _adjUL = do
      pos <- dPos
      let posUL = runMove UL 1 pos
      fmapMaybe (\(t, p) -> if p == posUL then Just t else Nothing) <$> thingsPos
  , _adjU = do
      pos <- dPos
      let posU = runMove U 1 pos
      fmapMaybe (\(t, p) -> if p == posU then Just t else Nothing) <$> thingsPos
  , _adjUR = do
      pos <- dPos
      let posUR = runMove UR 1 pos
      fmapMaybe (\(t, p) -> if p == posUR then Just t else Nothing) <$> thingsPos
  , _adjR = do
      pos <- dPos
      let posR = runMove R 1 pos
      fmapMaybe (\(t, p) -> if p == posR then Just t else Nothing) <$> thingsPos
  , _adjDR = do
      pos <- dPos
      let posDR = runMove DR 1 pos
      fmapMaybe (\(t, p) -> if p == posDR then Just t else Nothing) <$> thingsPos
  , _adjD = do
      pos <- dPos
      let posD = runMove D 1 pos
      fmapMaybe (\(t, p) -> if p == posD then Just t else Nothing) <$> thingsPos
  , _adjDL = do
      pos <- dPos
      let posDL = runMove DL 1 pos
      fmapMaybe (\(t, p) -> if p == posDL then Just t else Nothing) <$> thingsPos
  }
  where
    thingsPos :: Dynamic t [(Thing t, Pos)]
    thingsPos = do
      ts :: [Thing t] <- things
      let
        ts' :: [Dynamic t (Thing t, Pos)] = (\t -> (,) t <$> _thingPos t) <$> ts
        ts'' :: Dynamic t [(Thing t, Pos)] = distributeListOverDyn ts'
      ts''

data Thing t
  = Thing
  { _thingSprite :: Dynamic t Char
  , _thingPos :: Dynamic t Pos
  , _thingAdjacent :: Adjacent (Dynamic t) t
  , _thingHealth :: Dynamic t Int
  , _thingStatus :: Dynamic t Status
  , _thingAction :: Event t (NonEmpty Action)
  }

makePos ::
  (Reflex t, MonadHold t m, MonadFix m) =>
  Event t (NonEmpty Action) ->
  Pos ->
  Adjacent (Dynamic t) t ->
  m (Dynamic t Pos)
makePos eAction initialPos adj =
  foldDyn goPos initialPos $ (,) <$> current adj' <@> eAction
  where
    adj' = distAdjacentI adj

    goPos :: (Adjacent Identity t, NonEmpty Action) -> Pos -> Pos
    goPos (adj'', acts) p =
      foldr
        (\a b ->
           case a of
             Move dir dist ->
               case selectAdj dir adj'' of
                 Identity [] -> runMove dir dist b
                 _ -> b
             MoveTo pos -> pos
             _ -> b)
        p
        acts

    selectAdj L = _adjL
    selectAdj UL = _adjUL
    selectAdj U = _adjU
    selectAdj UR = _adjUR
    selectAdj R = _adjR
    selectAdj DR = _adjDR
    selectAdj D = _adjD
    selectAdj DL = _adjDL

data KThing a where
  KPlayer :: KThing (NonEmpty Action)
  KThing :: Int -> KThing (NonEmpty Action)
deriveGEq ''KThing
deriveGCompare ''KThing

mkThing ::
  (Reflex t, MonadHold t m, MonadFix m) =>
  Dynamic t [Thing t] -> -- ^ the other things that exist
  Pos -> -- ^ initial position
  Int -> -- ^ initial health
  Dynamic t Char -> -- ^ sprite
  Event t Int -> -- ^ damage
  Event t (NonEmpty Action) ->
  m (Thing t)
mkThing dThings pos health dSprite eDamage eAction = do
  rec
    let adj = mkAdjacent dPos dThings
    dPos <- makePos eAction pos adj
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
    , _thingAdjacent = adj
    , _thingHealth = dHealth
    , _thingStatus = dStatus
    , _thingAction = eAction
    }

makeLenses ''Thing
makeLenses ''Adjacent
