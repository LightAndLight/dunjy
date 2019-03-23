{-# language BangPatterns #-}
{-# language DataKinds #-}
{-# language FlexibleContexts, TypeFamilies #-}
{-# language GADTs, RankNTypes #-}
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
  , attachWith, fanMap
  )
import Reflex.Dynamic
  (Dynamic, switchDyn, updated, distributeMapOverDynPure, current, foldDyn)
import Reflex.Brick (ReflexBrickApp(..), switchReflexBrickApp)
import Reflex.Brick.Events (RBEvent(..))
import Reflex.Brick.Types (ReflexBrickAppState(..))
import Reflex.Collection (listHoldWithKey)
import Reflex.Host.Basic (BasicGuest, BasicGuestConstraints)
import Reflex.PostBuild.Class (PostBuild, getPostBuild)
import Reflex.Requester.Class (Requester, Request, Response, requesting)
import Reflex.Workflow (Workflow(..), workflow)

import Brick.AttrMap (attrMap)
import Brick.Widgets.Border (border)
import Brick.Widgets.Core ((<+>), (<=>), raw, txt, translateBy, vBox)
import Brick.Widgets.Dialog (dialog, renderDialog)
import Brick.Types (Widget, Location(..))
import Control.Applicative ((<|>), liftA2)
import Control.Monad.Fix (MonadFix)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (MonadReader, runReaderT, asks)
import Data.Dependent.Map (DMap)
import Data.Functor ((<&>))
import Data.Functor.Identity (Identity(..))
import Data.Functor.Misc (Const2(..))
import Data.Map (Map)
import Data.Text (Text)
import Graphics.Vty.Image (Image, backgroundFill)
import Graphics.Vty.Input.Events (Key(..))
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

renderActionLog :: Int -> [Map ThingType (DMap Action Identity)] -> Widget n
renderActionLog limit acts =
  border . vBox . take limit $ acts >>= renderRound
  where
    renderRound :: Map ThingType (DMap Action Identity) -> [Widget n]
    renderRound = Map.foldrWithKey (\k -> (<>) . renderActions k) []

    renderThingType :: ThingType -> Text
    renderThingType TPlayer = "the player"
    renderThingType (TThing n) = "mob " <> Text.pack (show n)

    renderAction :: ThingType -> Action a -> a -> Text
    renderAction tt k v =
      case k of
        Move d ->
          renderThingType tt <>
          " moved " <>
          Text.pack (show d)
        MoveTo ->
          renderThingType tt <>
          " moved to " <>
          Text.pack (show v)
        Melee ->
          renderThingType tt <>
          " attacked " <>
          Text.pack (show v)
        Wait ->
          renderThingType tt <>
          " did nothing"

    renderActions :: ThingType -> DMap Action Identity -> [Widget n]
    renderActions tt =
      DMap.foldrWithKey
        (\k (Identity v) rest -> txt (renderAction tt k v) : rest)
        []

makeAppState ::
  Reflex t =>
  Level t ->
  Dynamic t (Map ThingType (Pos, Char)) -> -- ^ mob position and sprite
  Dynamic t [Map ThingType (DMap Action Identity)] ->
  Dynamic t (ReflexBrickAppState n)
makeAppState level dMobs dActionLog =
  (\mobs actionLog ->
   ReflexBrickAppState
   { _rbWidgets =
     [ border (raw $ drawLevel level) <+>
       (border (txt $ "[s] Give me something to fight!") <=>
        border (txt $ "Mobs: " <> Text.pack (show $ Map.size mobs)) <=>
        renderActionLog 10 actionLog)
     ] <>
     foldr
       (\(Pos x y, s) b ->
          translateBy
            (Location (x, y))
            (txt $ Text.singleton s) :
          b)
       []
       mobs
   , _rbCursorFn = const Nothing
   , _rbAttrMap = attrMap mempty []
   }) <$>
  dMobs <*>
  dActionLog

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

    ePostBuild <- getPostBuild

    let
      eInsertPlayer :: Event t (Map ThingType (Maybe (Pos, Health)))
      eInsertPlayer = Map.singleton TPlayer (Just (Pos 1 1, Health 10)) <$ ePostBuild

      eInsertMob :: Event t (Map ThingType (Maybe (Pos, Health)))
      eInsertMob =
        eRandomPos <&>
        \(HCons1 (ResponseRandom p) (HCons1 (ResponseId i) HNil1)) ->
          Map.singleton (TThing i) (Just (p, Health 10))

    rec
      level <- newLevel 80 30 $ \x y -> pure (newTileAt $ Pos x y)

      let
        ePlayerUpdate :: Event t Updates
        ePlayerUpdate = select eMobsMoved' (Const2 TPlayer)

        dMobPositions :: Dynamic t (Map ThingType Pos)
        dMobPositions = dMobs >>= distributeMapOverDynPure . fmap _thingPos

        dMobPosAndSprite :: Dynamic t (Map ThingType (Pos, Char))
        dMobPosAndSprite =
          dMobs >>=
          distributeMapOverDynPure . fmap (liftA2 (,) <$> _thingPos <*> _thingSprite)

        -- dMobHealth :: Dynamic t (Map ThingType Health)
        -- dMobHealth = dMobs >>= distributeMapOverDynPure . fmap _thingHealth

        (eTick, mkPlayer) =
          initPlayer
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
          dMobPositions

      let
        eActions :: Event t (Map ThingType (DMap Action Identity))
        eActions = switchDyn $ mergeMap . fmap _thingAction <$> dMobs

        -- eMobsMelee :: Event t (Map ThingType ((Pos, Health), Dir))
        -- eMobsMelee = filterActionsWith (_thingPos &&& _thingHealth) meleeAction

      {-
        eDeleteMobs :: forall x. Event t (Map ThingType (Maybe x))
        eDeleteMobs =
          fmapMaybe (\h -> if h <= Health 0 then Just Nothing else Nothing) <$>
          updated dMobHealth
-}

        eMobsMoved :: Event t (Map ThingType Updates)
        eMobsMoved =
          attachWith
            (\a b ->
               moveThings (const True) $
               Map.mapWithKey (\k p -> (p, Map.lookup k b)) a)
            (current dMobPositions)
            (fmapMaybe moveAction <$> eActions)

        eMobsMoved' :: EventSelector t (Const2 ThingType Updates)
        eMobsMoved' = fanMap eMobsMoved

      dMobs :: Dynamic t (Map ThingType (Thing t (Dynamic t))) <-
        listHoldWithKey
          mempty
          (mergeWith (Map.unionWith (<|>)) [eInsertPlayer, eInsertMob{-, eDeleteMobs-}])
          (\k (pos, health) ->
             case k of
               TPlayer -> mkPlayer pos health ePlayerUpdate
               TThing{} -> do
                 initialDir <- random <$> liftIO newStdGen
                 dRandomDir :: Dynamic t (Optional Dir) <-
                   fmap fst <$> foldDyn (\_ -> random . snd) initialDir eTick

                 let
                   decision None = DMap.singleton Wait (pure ())
                   decision (Some dir) = DMap.singleton (Move dir) (pure ())

                   eAction = decision <$> updated dRandomDir

                   eUpdate :: Event t Updates
                   eUpdate = select eMobsMoved' (Const2 k)

                 mkThing pos health (pure 'Z') eAction eUpdate)

    dActionLog :: Dynamic t [Map ThingType (DMap Action Identity)] <- foldDyn (:) [] eActions

    pure
      ( ReflexBrickApp
        { rbaAppState = makeAppState level dMobPosAndSprite dActionLog
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
