{-# language FlexibleContexts, TypeFamilies #-}
{-# language OverloadedLists #-}
{-# language OverloadedStrings #-}
{-# language RecursiveDo #-}
{-# language ScopedTypeVariables #-}
{-# language TemplateHaskell #-}
module Dunjy where

import Reflex.Class
  ( Reflex, Event, EventSelector, MonadHold
  , select, never
  )
import Reflex.Dynamic (Dynamic, holdDyn)
import Reflex.Brick (ReflexBrickApp(..), switchReflexBrickApp)
import Reflex.Brick.Events (RBEvent(..))
import Reflex.Brick.Types (ReflexBrickAppState(..))
import Reflex.EventWriter.Base (runEventWriterT)
import Reflex.Host.Basic (BasicGuest, BasicGuestConstraints)
import Reflex.Requester.Class (Requester, Request, Response, requesting)
import Reflex.Workflow (Workflow(..), workflow)

import Brick.AttrMap (attrMap)
import Brick.Widgets.Border (border)
import Brick.Widgets.Core ((<+>), (<=>), raw, txt, translateBy)
import Brick.Widgets.Dialog (dialog, renderDialog)
import Brick.Types (Location(..))
import Control.Monad.Fix (MonadFix)
import Data.List.NonEmpty (NonEmpty)
import Data.Text (Text)
import Graphics.Vty.Image (Image, backgroundFill)
import Graphics.Vty.Input.Events (Key(..))
import Lens.Micro ((^.))

import qualified Data.Text as Text

import Thing
import Player
import Pos
import Random

newtype Screen = Screen Text

blankScreen :: Image
blankScreen = backgroundFill 80 80

initialState :: ReflexBrickAppState n
initialState =
  ReflexBrickAppState
  { _rbWidgets =
    [ renderDialog (dialog (Just "dunjy") Nothing 50) (txt "Press [space] to start")
    ]
  , _rbCursorFn = const Nothing
  , _rbAttrMap = attrMap mempty []
  }

makeAppState ::
  Pos ->
  Char ->
  Int -> -- ^ random 1
  Int -> -- ^ random 2
  ReflexBrickAppState n
makeAppState (Pos x y) psprite r1 r2 =
  ReflexBrickAppState
  { _rbWidgets =
    [ border (raw blankScreen) <+>
      (border (txt $ "random 1: " <> Text.pack (show r1)) <=>
       border (txt $ "random 2: " <> Text.pack (show r2)))
    , translateBy (Location (x, y)) (txt $ Text.singleton psprite)
    ]
  , _rbCursorFn = const Nothing
  , _rbAttrMap = attrMap mempty []
  }

dAppState ::
  Reflex t =>
  Thing t ->
  Dynamic t Int -> -- ^ random 1
  Dynamic t Int -> -- ^ random 2
  Dynamic t (ReflexBrickAppState n)
dAppState player r1 r2 =
  makeAppState <$>
  (player ^. thingPos) <*>
  (player ^. thingSprite) <*>
  r1 <*>
  r2

playScreen ::
  ( Reflex t, MonadHold t m
  , Requester t m, Request m ~ RequestRandom, Response m ~ ResponseRandom
  ) =>
  Event t () ->
  Event t (NonEmpty (ThingAction t)) ->
  Thing t ->
  Workflow t m (ReflexBrickApp t ())
playScreen eQuit eAction player =
  Workflow $ do
    eR1 <- requesting $ RequestRandom 1 100 <$ eAction
    eR2 <- requesting $ RequestRandom 1 100 <$ eAction
    dR1 <- holdDyn 0 $ (\(ResponseRandom a) -> a) <$> eR1
    dR2 <- holdDyn 0 $ (\(ResponseRandom a) -> a) <$> eR2

    pure
      ( ReflexBrickApp
        { rbaAppState = dAppState player dR1 dR2
        , rbaSuspendAndResume = never
        , rbaHalt = eQuit
        }
      , never
      )

startScreen
  :: ( Reflex t, MonadHold t m, MonadFix m
     , Requester t m, Request m ~ RequestRandom, Response m ~ ResponseRandom
     )
  => EventSelector t (RBEvent () e)
  -> Workflow t m (ReflexBrickApp t ())
startScreen events =
  Workflow $ do
    let
      eKeyQ = select events (RBKey (KChar 'q'))
      eKeyH = select events (RBKey (KChar 'h'))
      eKeyJ = select events (RBKey (KChar 'j'))
      eKeyK = select events (RBKey (KChar 'k'))
      eKeyL = select events (RBKey (KChar 'l'))
      eKeySpace = select events (RBKey (KChar ' '))
      eKeyDot = select events (RBKey (KChar '.'))

      eQuit = () <$ eKeyQ

    rec
      (player, eAction) <-
        runEventWriterT $ do
          mkPlayer $
            PlayerControls
            { _pcUp = () <$ eKeyK
            , _pcDown = () <$ eKeyJ
            , _pcLeft = () <$ eKeyH
            , _pcRight = () <$ eKeyL
            , _pcWait = () <$ eKeyDot
            }

    pure $
      ( ReflexBrickApp
        { rbaAppState = pure initialState
        , rbaSuspendAndResume = never
        , rbaHalt = eQuit
        }
      , playScreen eQuit eAction player <$ eKeySpace
      )

dunjy ::
  BasicGuestConstraints t m =>
  EventSelector t (RBEvent () e) ->
  BasicGuest t m (ReflexBrickApp t ())
dunjy events =
  runRandomT $
  switchReflexBrickApp <$> workflow (startScreen events)
