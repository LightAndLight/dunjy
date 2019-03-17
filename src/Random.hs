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

import Control.Monad.Fix (MonadFix)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Coerce (coerce)
import System.Random (Random, randomRIO)

data RequestRandom a where
  RequestRandom :: Random a => a -> a -> RequestRandom a

data ResponseRandom a where
  ResponseRandom :: a -> ResponseRandom a

newtype RandomT t m a
  = RandomT
  { unRandomT :: RequesterT t RequestRandom ResponseRandom m a
  } deriving (Functor, Applicative, Monad, MonadFix, MonadSample t, MonadHold t)

instance (MonadHold t m, MonadFix m, Adjustable t m) => Adjustable t (RandomT t m) where
  runWithReplace a b = RandomT (runWithReplace (coerce a) (coerceEvent b))
  traverseIntMapWithKeyWithAdjust a b c =
    RandomT (traverseIntMapWithKeyWithAdjust (coerce a) b c)
  traverseDMapWithKeyWithAdjustWithMove a b c =
    RandomT (traverseDMapWithKeyWithAdjustWithMove (coerce a) b c)

instance (Reflex t, Monad m) => Requester t (RandomT t m) where
  type Request (RandomT t m) = RequestRandom
  type Response (RandomT t m) = ResponseRandom

  requesting = RandomT . requesting
  requesting_ = RandomT . requesting_

runRandomT ::
  forall t m a.
  (Reflex t, PerformEvent t m, MonadIO (Performable m), MonadFix m) =>
  RandomT t m a ->
  m a
runRandomT (RandomT m) = do
  rec
    (a, eRequest) <- runRequesterT m eResponse
    let
      eRequest' :: Event t (Performable m (RequesterData ResponseRandom)) =
        fmap
        (traverseRequesterData
           (\case
              RequestRandom lower upper ->
                ResponseRandom <$> liftIO (randomRIO (lower, upper))))
        eRequest
    eResponse <- performEvent eRequest'
  pure a