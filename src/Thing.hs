{-# language FlexibleContexts #-}
{-# language TemplateHaskell #-}
module Thing where

import Reflex.Class (Reflex, Event, MonadHold)
import Reflex.Dynamic (Dynamic, foldDyn)
import Reflex.EventWriter.Class (EventWriter, tellEvent)

import Control.Monad.Fix (MonadFix)
import Data.Function ((&))
import Data.List.NonEmpty (NonEmpty)
import Lens.Micro ((%~))
import Lens.Micro.TH (makeLenses)

import Action
import Pos

data Thing t
  = Thing
  { _thingSprite :: Dynamic t Char
  , _thingPos :: Dynamic t Pos
  , _thingAction :: Event t (NonEmpty Action)
  }
makeLenses ''Thing

data ThingAction t
  = ThingAction (Thing t) Action

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
                 R -> b & posX %~ (+ dist)
                 U -> b & posY %~ subtract dist
                 D -> b & posY %~ (+ dist)
             MoveTo pos -> pos
             _ -> b)
        p
        acts

mkThing ::
  ( Reflex t, MonadHold t m, MonadFix m
  , EventWriter t (NonEmpty (ThingAction t)) m
  ) =>
  Pos -> -- ^ initial position
  Dynamic t Char ->
  Event t (NonEmpty Action) ->
  m (Thing t)
mkThing pos dSprite eAction = do
  dPos <- makePos eAction pos
  let
    res =
      Thing
      { _thingSprite = dSprite
      , _thingPos = dPos
      , _thingAction = eAction
      }
  res <$ tellEvent (fmap (ThingAction res) <$> eAction)
