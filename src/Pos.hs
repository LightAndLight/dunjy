{-# language TemplateHaskell #-}
module Pos where

import Reflex.Dynamic (Dynamic)

import Lens.Micro.TH (makeLenses)

data Pos = Pos { _posX :: !Int, _posY :: !Int }
  deriving (Eq, Show)

makeLenses ''Pos

data Positioned t a = Positioned { _posThing :: a, _posPos :: Dynamic t Pos }

makeLenses ''Positioned
