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
  , attachWith
  )
import Reflex.Collection (listHoldWithKey)
import Reflex.Dynamic
  (Dynamic, switchDyn, updated, joinDynThroughMap, current, foldDyn)
import Reflex.Brick (ReflexBrickApp(..), switchReflexBrickApp)
import Reflex.Brick.Events (RBEvent(..))
import Reflex.Brick.Types (ReflexBrickAppState(..))
import Reflex.Host.Basic (BasicGuest, BasicGuestConstraints)
import Reflex.PostBuild.Class (PostBuild, getPostBuild)
import Reflex.Requester.Class (Requester, Request, Response, requesting)
import Reflex.Workflow (Workflow(..), workflow)

import Brick.AttrMap (attrMap)
import Brick.Widgets.Border (border)
import Brick.Widgets.Core ((<+>), (<=>), raw, txt, translateBy)
import Brick.Widgets.Dialog (dialog, renderDialog)
import Brick.Types (Location(..))
import Control.Applicative ((<|>))
import Control.Monad.Fix (MonadFix)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (MonadReader, runReaderT, asks)
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.Map (Map)
import Data.Text (Text)
import Graphics.Vty.Image (Image, backgroundFill)
import Graphics.Vty.Input.Events (Key(..))
import Lens.Micro ((^.), (.~))
import System.Random (Random(..), newStdGen)

import qualified Data.Dependent.Map as DMap
import qualified Data.Map as Map
import qualified Data.Text as Text

import Action
import Level
import Movement
import Thing
import ThingType
import Tile
import Player
import Pos
import Random

newtype Screen = Screen Text

screenWidth :: Int
screenWidth = 80

screenHeight :: Int
screenHeight = 80

initialState :: ReflexBrickAppState n
initialState =
  ReflexBrickAppState
  { _rbWidgets =
    [ renderDialog (dialog (Just "dunjy") Nothing 50) (txt "Press [space] to start")
    ]
  , _rbCursorFn = const Nothing
  , _rbAttrMap = attrMap mempty []
  }

drawLevel :: Level t -> Image
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
  Level t ->
  Dynamic t (Map ThingType (Thing t)) ->
  Dynamic t (ReflexBrickAppState n)
makeAppState level dMobs =
  (\mobs ->
   ReflexBrickAppState
   { _rbWidgets =
     [ border (raw $ drawLevel level) <+>
       (border (txt $ "[s] Give me something to fight!") <=>
        border (txt $ "Mobs: " <> Text.pack (show $ Map.size mobs)))
     ] <>
     foldr
       (\(Pos x y, s) b ->
          translateBy (Location (x, y)) (txt $ Text.singleton s) : b)
       []
       mobs
   , _rbCursorFn = const Nothing
   , _rbAttrMap = attrMap mempty []
   }) <$>
  joinDynThroughMap (fmap (\t -> (,) (t ^. thingPos) <$> (^.) t thingSprite) <$> dMobs)

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
  , PostBuild t m
  , MonadReader (EventSelector t (RBEvent () e)) m
  , MonadIO m
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

    eRandomPos :: Event t (HList1 DunjyResponse (L [RRandom Pos, RId Int])) <-
      requesting $
      (HCons1 (RequestRandomR (Pos 1 1) (Pos 79 29)) $
       HCons1 RequestId $
       HNil1) <$
      eKeyS

    let
      eInsertMob =
        eRandomPos <&>
        \(HCons1 (ResponseRandom p) (HCons1 (ResponseId i) HNil1)) ->
          Map.singleton (TThing i) (Just p)

    ePostBuild <- getPostBuild

    rec
      level <- newLevel 80 30 $ \x y -> pure (newTileAt $ Pos x y)

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
          (Pos 0 0)
          dMobs
          never

      let
        eInsertPlayer = Map.singleton TPlayer (Just $ Pos 1 1) <$ ePostBuild

        eDeleteMobs :: Event t (Map ThingType (Maybe Pos))
        eDeleteMobs =
          switchDyn $
          mergeMap .
          fmap
            (fmapMaybe (\s -> if s == Dead then Just Nothing else Nothing) .
             updated .
             _thingStatus) <$>
          dMobs

        eMobsMoved :: Event t (Map ThingType (Maybe Pos))
        eMobsMoved =
          attachWith
            (\a b ->
               fmap Just .
               moveThings (const True) $
               b <> fmap (\t -> (t ^. thingPos, Nothing)) a)
            (current dMobs)
            (switchDyn $
             mergeMap .
             fmap
               (\t ->
                  fmapMaybe
                    (fmap ((,) (t ^. thingPos) . Just) . moveAction)
                    (t ^. thingAction)) <$>
             dMobs)

      dMobs :: Dynamic t (Map ThingType (Thing t)) <-
        listHoldWithKey
          mempty
          (mergeWith (Map.unionWith (<|>)) [eInsertPlayer, eInsertMob, eDeleteMobs, eMobsMoved])
          (\k pos ->
             case k of
               TPlayer -> pure $ playerThing & thingPos .~ pos
               TThing{} -> do
                 initialDir <- random <$> liftIO newStdGen
                 dRandomDir :: Dynamic t (Optional Dir) <-
                   fmap fst <$> foldDyn (\_ -> random . snd) initialDir eTick

                 let
                   decision None = DMap.singleton Wait (pure ())
                   decision (Some dir) = DMap.singleton (Move dir) (pure ())

                   eAction = decision <$> updated dRandomDir

                 thing <- mkThing pos 10 (pure 'Z') never eAction

                 pure thing)

    pure
      ( ReflexBrickApp
        { rbaAppState = makeAppState level dMobs
        , rbaSuspendAndResume = never
        , rbaHalt = eQuit
        }
      , never
      )

startScreen ::
  ( Reflex t, MonadHold t m, MonadFix m
  , Requester t m
  , Request m ~ HList1 DunjyRequest, Response m ~ HList1 DunjyResponse
  , Adjustable t m
  , PostBuild t m
  , MonadReader (EventSelector t (RBEvent () e)) m
  , MonadIO m
  ) =>
  Workflow t m (ReflexBrickApp t ())
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
