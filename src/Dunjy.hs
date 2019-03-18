{-# language DataKinds #-}
{-# language FlexibleContexts, TypeFamilies #-}
{-# language LambdaCase #-}
{-# language OverloadedLists #-}
{-# language OverloadedStrings #-}
{-# language RecursiveDo #-}
{-# language ScopedTypeVariables #-}
{-# language TemplateHaskell #-}
module Dunjy where

import Reflex.Class
  ( Reflex, Event, EventSelector, MonadHold
  , select, never, mergeWith
  )
import Reflex.Collection (listHoldWithKey)
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
import Control.Monad.Reader (MonadReader, runReaderT, asks)
import Data.Dependent.Sum ((==>))
import Data.Functor ((<&>))
import Data.Text (Text)
import Graphics.Vty.Image (Image, backgroundFill)
import Graphics.Vty.Input.Events (Key(..))
import Lens.Micro ((^.))
import System.Random (Random(..))

import qualified Data.Map as Map
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
  Reflex t =>
  Thing t ->
  Dynamic t (ReflexBrickAppState n)
makeAppState player =
  (\(Pos x y) psprite ->
   ReflexBrickAppState
   { _rbWidgets =
     [ border (raw blankScreen) <+>
       border (txt $ "[s] Give me something to fight!")
     , translateBy (Location (x, y)) (txt $ Text.singleton psprite)
     ]
   , _rbCursorFn = const Nothing
   , _rbAttrMap = attrMap mempty []
   }) <$>
  (player ^. thingPos) <*>
  (player ^. thingSprite)

data Pair a b = Pair a b
  deriving (Eq, Show)

instance (Random a, Random b) => Random (Pair a b) where
  randomR ((Pair alo blo), (Pair ahigh bhigh)) g =
    let
      (aval, g') = randomR (alo, ahigh) g
      (bval, g'') = randomR (blo, bhigh) g'
    in
      (Pair aval bval, g'')
  random g =
    let
      (aval, g') = random g
      (bval, g'') = random g'
    in
      (Pair aval bval, g'')

playScreen ::
  ( Reflex t, MonadHold t m
  , Requester t m, Request m ~ HList1 DunjyRequest, Response m ~ HList1 DunjyResponse
  , MonadReader (EventSelector t (RBEvent () e)) m
  ) =>
  Event t () ->
  Event t () ->
  Thing t ->
  Workflow t m (ReflexBrickApp t ())
playScreen eQuit eTick player =
  Workflow $ do
    eKeyS <- askSelect $ RBKey (KChar 's')

    eRandomPos :: Event t (HList1 DunjyResponse (L [Pair Int Int, Int])) <-
      requesting $
      (HCons1 (RequestRandom (Pair (1 :: Int) (1 :: Int)) (Pair 80 80)) $
       HCons1 RequestId $
       HNil1) <$
      eKeyS

    let
      eInsertMob = eRandomPos
      eDeleteMob = _

    dMobs <-
      listHoldWithKey
        mempty
        (mergeWith
           (Map.unionWith _)
           [ eInsertMob <&> \(HCons1 a (HCons1 b HNil1)) ->
               _
           , eDeleteMob
           ])
        _

    pure
      ( ReflexBrickApp
        { rbaAppState = makeAppState player
        , rbaSuspendAndResume = never
        , rbaHalt = eQuit
        }
      , never
      )

askSelect :: MonadReader (EventSelector t k) m => k a -> m (Event t a)
askSelect k = asks (`select` k)

startScreen
  :: ( Reflex t, MonadHold t m, MonadFix m
     , Requester t m
     , Request m ~ HList1 DunjyRequest, Response m ~ HList1 DunjyResponse
     , MonadReader (EventSelector t (RBEvent () e)) m
     )
  => Workflow t m (ReflexBrickApp t ())
startScreen =
  Workflow $ do
    eKeyQ <- askSelect $ RBKey (KChar 'q')
    eKeyH <- askSelect $ RBKey (KChar 'h')
    eKeyJ <- askSelect $ RBKey (KChar 'j')
    eKeyK <- askSelect $ RBKey (KChar 'k')
    eKeyL <- askSelect $ RBKey (KChar 'l')
    eKeySpace <- askSelect $ RBKey (KChar ' ')
    eKeyDot <- askSelect $ RBKey (KChar '.')

    let eQuit = () <$ eKeyQ

    (eTick, eAction, player) <-
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
      , playScreen eQuit eTick player <$ eKeySpace
      )

dunjy ::
  BasicGuestConstraints t m =>
  EventSelector t (RBEvent () e) ->
  BasicGuest t m (ReflexBrickApp t ())
dunjy events =
  runRandomT $
  flip runReaderT events $
  switchReflexBrickApp <$> workflow startScreen
