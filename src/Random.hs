{-# language DataKinds, TypeOperators #-}
{-# language FlexibleContexts #-}
{-# language FlexibleInstances, MultiParamTypeClasses #-}
{-# language FunctionalDependencies #-}
{-# language GADTs #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language LambdaCase #-}
{-# language PolyKinds #-}
{-# language RankNTypes #-}
{-# language RecursiveDo #-}
{-# language ScopedTypeVariables #-}
{-# language TypeFamilies #-}
module Random where


import Reflex.Adjustable.Class (Adjustable(..))
import Reflex.Class (Reflex, Event, MonadSample, MonadHold, coerceEvent)
import Reflex.PerformEvent.Class (PerformEvent, Performable, performEvent)
import Reflex.Requester.Base (RequesterT, RequesterData, runRequesterT, traverseRequesterData)
import Reflex.Requester.Class (Requester(..))
import Reflex.PostBuild.Class (PostBuild)

import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TVar (newTVarIO, readTVar, writeTVar)
import Control.Concurrent.Supply (newSupply, freshId)
import Control.Monad.Fix (MonadFix)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Coerce (coerce)
import System.Random (Random, randomRIO, randomIO)

data RRandom a
data RId a

data DunjyRequest a where
  RequestRandomR :: Random a => a -> a -> DunjyRequest (RRandom a)
  RequestRandom :: Random a => DunjyRequest (RRandom a)
  RequestId :: DunjyRequest (RId Int)

data DunjyResponse a where
  ResponseRandom :: a -> DunjyResponse (RRandom a)
  ResponseId :: a -> DunjyResponse (RId a)

data L (l :: [*])

data HList1 (f :: * -> *) (ls :: *) where
  HNil1 :: HList1 f (L '[])
  HCons1 :: f x -> HList1 f (L xs) -> HList1 f (L (x ': xs))

traverseHList1 ::
  Applicative m =>
  (forall x. f x -> m (g x)) ->
  HList1 f as ->
  m (HList1 g as)
traverseHList1 _ HNil1 = pure HNil1
traverseHList1 f (HCons1 a b) = HCons1 <$> f a <*> traverseHList1 f b

newtype RandomT t m a
  = RandomT
  { unRandomT :: RequesterT t (HList1 DunjyRequest) (HList1 DunjyResponse) m a
  } deriving
  ( Functor, Applicative, Monad
  , MonadFix, MonadIO, MonadSample t, MonadHold t, PostBuild t
  )

instance (MonadHold t m, MonadFix m, Adjustable t m) => Adjustable t (RandomT t m) where
  runWithReplace a b = RandomT (runWithReplace (coerce a) (coerceEvent b))
  traverseIntMapWithKeyWithAdjust a b c =
    RandomT (traverseIntMapWithKeyWithAdjust (coerce a) b c)
  traverseDMapWithKeyWithAdjustWithMove a b c =
    RandomT (traverseDMapWithKeyWithAdjustWithMove (coerce a) b c)

instance (Reflex t, Monad m) => Requester t (RandomT t m) where
  type Request (RandomT t m) = HList1 DunjyRequest
  type Response (RandomT t m) = HList1 DunjyResponse

  requesting = RandomT . requesting
  requesting_ = RandomT . requesting_

runRandomT ::
  forall t m a.
  (Reflex t, PerformEvent t m, MonadIO (Performable m), MonadFix m, MonadIO m) =>
  RandomT t m a ->
  m a
runRandomT (RandomT m) = do
  supplyVar <- liftIO $ newTVarIO =<< newSupply
  rec
    (a, eRequest) <- runRequesterT m eResponse
    let
      eRequest' :: Event t (Performable m (RequesterData (HList1 DunjyResponse))) =
        fmap
        (traverseRequesterData $
         traverseHList1
           (\case
              RequestRandomR lower upper ->
                ResponseRandom <$>
                liftIO (randomRIO (lower, upper))
              RequestRandom ->
                ResponseRandom <$> liftIO randomIO
              RequestId ->
                fmap ResponseId .
                liftIO .
                atomically $ do
                  s <- readTVar supplyVar
                  let (i, s') = freshId s
                  writeTVar supplyVar s'
                  pure i))
        eRequest
    eResponse <- performEvent eRequest'
  pure a
