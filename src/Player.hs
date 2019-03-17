{-# language FlexibleContexts #-}
{-# language ScopedTypeVariables #-}
{-# language TemplateHaskell #-}
module Player where

import Reflex.Class (Reflex, Event, MonadHold, mergeList)
import Reflex.EventWriter.Class (EventWriter)

import Control.Monad.Fix (MonadFix)
import Data.List.NonEmpty (NonEmpty)
import Lens.Micro ((^.))
import Lens.Micro.TH (makeLenses)

import Action
import Thing
import Pos

data PlayerControls t
  = PlayerControls
  { _pcUp :: Event t ()
  , _pcDown :: Event t ()
  , _pcLeft :: Event t ()
  , _pcRight :: Event t ()
  , _pcWait :: Event t ()
  }
makeLenses ''PlayerControls

mkPlayer ::
  forall t m.
  ( Reflex t, MonadHold t m, MonadFix m
  , EventWriter t (NonEmpty (ThingAction t)) m
  ) =>
  PlayerControls t ->
  m (Event t (), Thing t)
mkPlayer pc = do
  let
    eTick :: Event t (NonEmpty Action)
    eTick =
      mergeList
      [ Move U 1 <$ (pc ^. pcUp)
      , Move D 1 <$ (pc ^. pcDown)
      , Move L 1 <$ (pc ^. pcLeft)
      , Move R 1 <$ (pc ^. pcRight)
      , Wait <$ (pc ^. pcWait)
      ]

    eAction = eTick

  res <- mkThing (Pos 1 1) (pure '@') eAction
  pure (() <$ eTick, res)
