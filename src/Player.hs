{-# language FlexibleContexts #-}
{-# language ScopedTypeVariables #-}
{-# language TemplateHaskell #-}
module Player where

import Reflex.Class (Reflex, Event, MonadHold, merge)

import Control.Monad.Fix (MonadFix)
import Data.Dependent.Map (DMap)
import Data.Dependent.Sum (DSum(..))
import Data.Functor.Identity (Identity)
import Lens.Micro ((^.))
import Lens.Micro.TH (makeLenses)

import qualified Data.Dependent.Map as DMap

import Action
import Thing

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

mkPlayer ::
  forall t m.
  ( Reflex t, MonadHold t m, MonadFix m
  ) =>
  PlayerControls t -> -- ^ controls
  Event t Int -> -- ^ received damage
  m (Event t (), Thing t)
mkPlayer pc eDamage = do
  let
    eTick :: Event t (DMap Action Identity)
    eTick =
      merge . DMap.fromList $
      [ Move L  :=> () <$ (pc ^. pcLeft)
      , Move UL :=> () <$ (pc ^. pcUpLeft)
      , Move U  :=> () <$ (pc ^. pcUp)
      , Move UR :=> () <$ (pc ^. pcUpRight)
      , Move R  :=> () <$ (pc ^. pcRight)
      , Move DR :=> () <$ (pc ^. pcDownRight)
      , Move D  :=> () <$ (pc ^. pcDown)
      , Move DL :=> () <$ (pc ^. pcDownLeft)
      , Wait :=> (pc ^. pcWait)
      ]

    eAction = eTick

  res <- mkThing 10 (pure '@') eDamage eAction
  pure (() <$ eTick, res)
