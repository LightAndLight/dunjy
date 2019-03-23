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
  ( Reflex, Event, EventSelector, MonadHold, FunctorMaybe
  , select, never, mergeWith, mergeMap
  , fanMap, fmapMaybe
  , coerceEvent, fmapCheap
  , (<@>), ffilter
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
import Control.Lens.Fold (folded, notNullOf)
import Control.Lens.Getter ((^.))
import Control.Lens.Setter ((?~))
import Control.Monad.Fix (MonadFix)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (MonadReader, runReaderT, asks)
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.Functor.Identity (Identity)
import Data.Functor.Misc (Const2(..))
import Data.Map (Map)
import Data.Map.Monoidal (MonoidalMap(..))
import Data.Text (Text)
import Graphics.Vty.Image (Image, backgroundFill)
import Graphics.Vty.Input.Events (Key(..))
import System.Random (Random(..), newStdGen)

import qualified Data.Map as Map
import qualified Data.Text as Text

import Data.Monoid.Action (act)
import Action
import Attack
import Level
import Movement
import Thing
import ThingType
import Tile
import Player
import Pos
import Random

newtype Screen = Screen Text

fmapMaybeCompose ::
  (Reflex t, Foldable f, FunctorMaybe f) =>
  (a -> Maybe b) ->
  Event t (f a) ->
  Event t (f b)
fmapMaybeCompose f =
  fmapMaybe (\m -> let res = fmapMaybe f m in if null res then Nothing else Just res)

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

renderActionLog :: Int -> [Map ThingType Action] -> Widget n
renderActionLog limit acts =
  border . vBox . take limit $ acts >>= renderRound
  where
    renderRound :: Map ThingType Action -> [Widget n]
    renderRound = Map.foldrWithKey (\k -> (:) . renderAction k) []

    renderThingType :: ThingType -> Text
    renderThingType TPlayer = "the player"
    renderThingType (TThing n) = "mob " <> Text.pack (show n)

    renderAction :: ThingType -> Action -> Widget n
    renderAction tt k =
      txt $
      case k of
        Move (Relative d) ->
          renderThingType tt <>
          " moved " <>
          Text.pack (show d)
        Move (Absolute v) ->
          renderThingType tt <>
          " moved to " <>
          Text.pack (show v)
        Melee v ->
          renderThingType tt <>
          " attacked " <>
          Text.pack (show v)
        Wait ->
          renderThingType tt <>
          " did nothing"

makeAppState ::
  Reflex t =>
  Level t ->
  Dynamic t (Map ThingType (Pos, Char)) -> -- ^ mob position and sprite
  Dynamic t [Map ThingType Action] ->
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
          Map.singleton (TThing i) (Just (p, Health 2))

    rec
      level <- newLevel 80 30 $ \x y -> pure (newTileAt $ Pos x y)

      let
        dMobs :: Dynamic t (Map ThingType (Thing t Identity))
        dMobs = _dMobs >>= distributeMapOverDynPure . fmap sequenceThing

        dMobPosAndSprite :: Dynamic t (Map ThingType (Pos, Char))
        dMobPosAndSprite =
          _dMobs >>=
          distributeMapOverDynPure . fmap (liftA2 (,) <$> _thingPos <*> _thingSprite)

        dMobPositions :: Dynamic t (Map ThingType Pos)
        dMobPositions = _dMobs >>= distributeMapOverDynPure . fmap _thingPos

        dMobHealth :: Dynamic t (Map ThingType Health)
        dMobHealth = _dMobs >>= distributeMapOverDynPure . fmap _thingHealth

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
        eActions :: Event t (Map ThingType Action)
        eActions = switchDyn $ mergeMap . fmap _thingAction <$> dMobs

        eMobsDamaged :: Event t (Map ThingType Damage)
        eMobsDamaged =
          runMelees <$>
          current dMobs <@>
          ffilter (notNullOf $ folded._Melee) eActions

        eMobsMoved :: Event t (MonoidalMap ThingType Updates)
        eMobsMoved =
          fmapCheap MonoidalMap $
          moveThings (const True) <$>
          current dMobs <@>
          ffilter (notNullOf $ folded._Move) eActions

        eMobsUpdated :: Event t (MonoidalMap ThingType Updates)
        eMobsUpdated =
          mergeWith (<>) $
          [ eMobsMoved
          , fmapCheap MonoidalMap $
            (\hs ->
               Map.foldrWithKey
                 (\k d ->
                    maybe
                      id
                      (\h -> Map.insert k $ mempty & _updateHealth ?~ act d h)
                      (Map.lookup k hs))
                 mempty) <$>
            current dMobHealth <@>
            eMobsDamaged
          ]

        eDeleteMobs :: Event t (Map ThingType (Maybe (Pos, Health)))
        eDeleteMobs =
          fmapMaybeCompose
            (\u ->
               maybe
                 Nothing
                 (\h -> if h <= Health 0 then Just Nothing else Nothing)
                 (u ^. _updateHealth))
            (fmapCheap getMonoidalMap eMobsUpdated)

        updateSelector :: EventSelector t (Const2 ThingType Updates)
        updateSelector = fanMap $ coerceEvent eMobsUpdated

        ePlayerUpdate :: Event t Updates
        ePlayerUpdate = select updateSelector (Const2 TPlayer)

      _dMobs :: Dynamic t (Map ThingType (Thing t (Dynamic t))) <-
        listHoldWithKey
          mempty
          (mergeWith (Map.unionWith (<|>)) [eInsertPlayer, eInsertMob, eDeleteMobs])
          (\k (pos, health) ->
             case k of
               TPlayer -> mkPlayer pos health ePlayerUpdate
               TThing{} -> do
                 initialDir <- random <$> liftIO newStdGen
                 dRandomDir :: Dynamic t (Optional Dir) <-
                   fmap fst <$> foldDyn (\_ -> random . snd) initialDir eTick

                 let
                   decision None = Wait
                   decision (Some dir) = Move $ Relative dir

                   eAction = decision <$> updated dRandomDir

                   eUpdate :: Event t Updates
                   eUpdate = select updateSelector (Const2 k)

                 mkThing pos health (pure 'Z') eAction eUpdate)

    dActionLog :: Dynamic t [Map ThingType Action] <- foldDyn (:) [] eActions

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
