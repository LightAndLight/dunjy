{-# language FlexibleContexts #-}
{-# language FlexibleInstances, MultiParamTypeClasses #-}
{-# language GADTs #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language LambdaCase #-}
{-# language RecursiveDo #-}
{-# language ScopedTypeVariables #-}
{-# language TypeFamilies #-}
module Random where

import Reflex.Adjustable.Class (Adjustable(..))
import Reflex.Class (Reflex, Event, MonadSample, MonadHold, coerceEvent)
import Reflex.PerformEvent.Class (PerformEvent, Performable, performEvent)
import Reflex.Requester.Base (RequesterT, RequesterData, runRequesterT, traverseRequesterData)
import Reflex.Requester.Class (Requester(..))

import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TVar (newTVarIO, readTVar, writeTVar)
import Control.Concurrent.Supply (newSupply, freshId)
import Control.Monad.Fix (MonadFix)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Coerce (coerce)
import System.Random (Random, randomRIO)

data DunjyRequest a where
  RequestRandom :: Random a => a -> a -> DunjyRequest a
  RequestId :: DunjyRequest Int

data DunjyResponse a where
  ResponseRandom :: a -> DunjyResponse a
  ResponseId :: a -> DunjyResponse a

newtype RandomT t m a
  = RandomT
  { unRandomT :: RequesterT t DunjyRequest DunjyResponse m a
  } deriving (Functor, Applicative, Monad, MonadFix, MonadSample t, MonadHold t)

instance (MonadHold t m, MonadFix m, Adjustable t m) => Adjustable t (RandomT t m) where
  runWithReplace a b = RandomT (runWithReplace (coerce a) (coerceEvent b))
  traverseIntMapWithKeyWithAdjust a b c =
    RandomT (traverseIntMapWithKeyWithAdjust (coerce a) b c)
  traverseDMapWithKeyWithAdjustWithMove a b c =
    RandomT (traverseDMapWithKeyWithAdjustWithMove (coerce a) b c)

instance (Reflex t, Monad m) => Requester t (RandomT t m) where
  type Request (RandomT t m) = DunjyRequest
  type Response (RandomT t m) = DunjyResponse

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
      eRequest' :: Event t (Performable m (RequesterData DunjyResponse)) =
        fmap
        (traverseRequesterData
           (\case
              RequestRandom lower upper ->
                ResponseRandom <$> liftIO (randomRIO (lower, upper))
              RequestId ->
                liftIO .
                atomically $ do
                  s <- readTVar supplyVar
                  let (i, s') = freshId s
                  writeTVar supplyVar s'
                  pure $ ResponseId i))
        eRequest
    eResponse <- performEvent eRequest'
  pure a
