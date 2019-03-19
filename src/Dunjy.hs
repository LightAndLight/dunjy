{-# language BangPatterns #-}
{-# language DataKinds #-}
{-# language FlexibleContexts, TypeFamilies #-}
{-# language LambdaCase #-}
{-# language OverloadedLists #-}
{-# language OverloadedStrings #-}
{-# language RecursiveDo #-}
{-# language ScopedTypeVariables #-}
{-# language TemplateHaskell #-}
module Dunjy where

import Reflex.Adjustable.Class (Adjustable)
import Reflex.Class
  ( Reflex, Event, EventSelector, MonadHold
  , select, never, mergeWith, mergeMap, fmapMaybe
  )
import Reflex.Collection (listHoldWithKey)
import Reflex.Dynamic
  (Dynamic, switchDyn, updated, joinDynThroughMap)
import Reflex.Brick (ReflexBrickApp(..), switchReflexBrickApp)
import Reflex.Brick.Events (RBEvent(..))
import Reflex.Brick.Types (ReflexBrickAppState(..))
import Reflex.Host.Basic (BasicGuest, BasicGuestConstraints)
import Reflex.Requester.Class (Requester, Request, Response, requesting)
import Reflex.Workflow (Workflow(..), workflow)

import Brick.AttrMap (attrMap)
import Brick.Widgets.Border (border)
import Brick.Widgets.Core ((<+>), raw, txt, translateBy)
import Brick.Widgets.Dialog (dialog, renderDialog)
import Brick.Types (Location(..))
import Control.Applicative ((<|>))
import Control.Monad.Fix (MonadFix)
import Control.Monad.Reader (MonadReader, runReaderT, asks)
import Data.Functor ((<&>))
import Data.List.NonEmpty (NonEmpty)
import Data.Map (Map)
import Data.Text (Text)
import Graphics.Vty.Image (Image, backgroundFill)
import Graphics.Vty.Input.Events (Key(..))
import Lens.Micro ((^.))
import System.Random (Random(..))

import qualified Data.Map as Map
import qualified Data.Text as Text

import Action
import Level
import Thing
import ThingType
import Tile
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

drawLevel :: Level t (Dynamic t) -> Image
drawLevel (Level w h _) = backgroundFill w h
{-
  go 0 0 mempty mempty
  where
    go !x !y img line =
      if x == w
      then
        if y == h
        then img <-> text mempty (Builder.toLazyText line)
        else go 0 (y+1) (img <-> text mempty (Builder.toLazyText line)) mempty
      else
        -- go (x+1) y img (line <> _ (Vector.unsafeIndex ts (y * w + x)))
        go (x+1) y img (line <> Builder.singleton ' ')
-}

makeAppState ::
  Reflex t =>
  Level t (Dynamic t) ->
  Positioned t (Thing t) ->
  Dynamic t (Map ThingType (Pos, Thing t)) ->
  Dynamic t (ReflexBrickAppState n)
makeAppState level player dMobs =
  (\(Pos x y) psprite mobs ->
   ReflexBrickAppState
   { _rbWidgets =
     [ border (raw $ drawLevel level) <+>
       border (txt $ "[s] Give me something to fight!")
     , translateBy (Location (x, y)) (txt $ Text.singleton psprite)
     ] <>
     foldr
       (\(Pos xx yy, s) b ->
          translateBy (Location (xx, yy)) (txt $ Text.singleton s) : b)
       []
       mobs
   , _rbCursorFn = const Nothing
   , _rbAttrMap = attrMap mempty []
   }) <$>
  (player ^. posPos) <*>
  (player ^. posThing.thingSprite) <*>
  joinDynThroughMap (fmap (\(p, t) -> (,) p <$> (^.) t thingSprite) <$> dMobs)

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

data Optional a
  = None
  | Some a
  deriving (Eq, Show, Ord)

instance (Bounded a, Random a) => Random (Optional a) where
  randomR (None, None) g = (None, g)
  randomR (None, Some a) g =
    let
      (b, g') = random g
    in
      if b
      then (None, g')
      else
        let
          (a', g'') = randomR (minBound, a) g'
        in
          (Some a', g'')
  randomR (Some a, Some b) g =
    let (a', g') = randomR (a, b) g in (Some a', g')
  randomR (Some a, None) g = (Some a, g)

  random = randomR (None, Some maxBound)

askSelect :: MonadReader (EventSelector t k) m => k a -> m (Event t a)
askSelect k = asks (`select` k)

playScreen ::
  ( Reflex t, MonadHold t m, MonadFix m
  , Requester t m, Request m ~ HList1 DunjyRequest, Response m ~ HList1 DunjyResponse
  , Adjustable t m
  , MonadReader (EventSelector t (RBEvent () e)) m
  ) =>
  Event t () ->
  Workflow t m (ReflexBrickApp t ())
playScreen eQuit =
  Workflow $ do
    eKeyH <- askSelect $ RBKey (KChar 'h')
    eKeyJ <- askSelect $ RBKey (KChar 'j')
    eKeyK <- askSelect $ RBKey (KChar 'k')
    eKeyL <- askSelect $ RBKey (KChar 'l')

    eKeyY <- askSelect $ RBKey (KChar 'y')
    eKeyU <- askSelect $ RBKey (KChar 'u')
    eKeyB <- askSelect $ RBKey (KChar 'b')
    eKeyN <- askSelect $ RBKey (KChar 'n')

    eKeyDot <- askSelect $ RBKey (KChar '.')

    eKeyS <- askSelect $ RBKey (KChar 's')

    eRandomPos :: Event t (HList1 DunjyResponse (L [RRandom (Pair Int Int), RId Int])) <-
      requesting $
      (HCons1 (RequestRandomR (Pair (1 :: Int) (1 :: Int)) (Pair 79 79)) $
       HCons1 RequestId $
       HNil1) <$
      eKeyS

    let
      eInsertMob =
        eRandomPos <&>
        \(HCons1 (ResponseRandom (Pair x y)) (HCons1 (ResponseId i) HNil1)) ->
          Map.singleton (TThing i) (Just (Pos x y))

    rec
      level <- newLevel 80 80 $ \x y -> pure (newTileAt dMobs' $ Pos x y)
      let dLevel = distLevelD level

      (eTick, playerThing) <-
        mkPlayer
          (PlayerControls
          { _pcLeft = () <$ eKeyH
          , _pcUpLeft = () <$ eKeyY
          , _pcUp = () <$ eKeyK
          , _pcUpRight = () <$ eKeyU
          , _pcRight = () <$ eKeyL
          , _pcDownRight = () <$ eKeyN
          , _pcDown = () <$ eKeyJ
          , _pcDownLeft = () <$ eKeyB
          , _pcWait = () <$ eKeyDot
          })
          never

      playerPos <- mkPos dLevel (playerThing ^. thingAction) (Pos 1 1)
      let player = Positioned playerThing playerPos

      let
        eDeleteMob =
          switchDyn $
          mergeMap .
          fmap
            (fmapMaybe (\s -> if s == Dead then Just Nothing else Nothing) .
             updated .
             (^. posThing.thingStatus)) <$>
          dMobs

      dMobs :: Dynamic t (Map ThingType (Positioned t (Thing t))) <-
        fmap (Map.insert TPlayer player) <$>
        listHoldWithKey
          mempty
          (mergeWith (Map.unionWith (<|>)) [eInsertMob, eDeleteMob])
          (\_ pos -> do
              eRand <- requesting $ HCons1 RequestRandom HNil1 <$ eTick
              let
                decision ::
                  HList1 DunjyResponse (L '[RRandom (Optional Dir)]) ->
                  NonEmpty Action
                decision (HCons1 (ResponseRandom odir) HNil1) =
                  case odir of
                    None -> [Wait]
                    Some dir -> [Move dir 1]

                eAction = decision <$> eRand

              thing <- mkThing 10 (pure 'Z') never eAction
              p <- mkPos dLevel (_thingAction thing) pos

              pure $ Positioned thing p)

      let
        dMobs' :: Dynamic t (Map ThingType (Pos, Thing t)) =
          joinDynThroughMap (fmap (\(Positioned t p) -> (,) <$> p <*> pure t) <$> dMobs)

    pure
      ( ReflexBrickApp
        { rbaAppState = makeAppState level player dMobs'
        , rbaSuspendAndResume = never
        , rbaHalt = eQuit
        }
      , never
      )

startScreen
  :: ( Reflex t, MonadHold t m, MonadFix m
     , Requester t m
     , Request m ~ HList1 DunjyRequest, Response m ~ HList1 DunjyResponse
     , Adjustable t m
     , MonadReader (EventSelector t (RBEvent () e)) m
     )
  => Workflow t m (ReflexBrickApp t ())
startScreen =
  Workflow $ do
    eKeyQ <- askSelect $ RBKey (KChar 'q')

    eKeySpace <- askSelect $ RBKey (KChar ' ')

    let eQuit = () <$ eKeyQ

    pure $
      ( ReflexBrickApp
        { rbaAppState = pure initialState
        , rbaSuspendAndResume = never
        , rbaHalt = eQuit
        }
      , playScreen eQuit <$ eKeySpace
      )

dunjy ::
  BasicGuestConstraints t m =>
  EventSelector t (RBEvent () e) ->
  BasicGuest t m (ReflexBrickApp t ())
dunjy events =
  runRandomT $
  flip runReaderT events $
  switchReflexBrickApp <$> workflow startScreen
