{-# language TemplateHaskell #-}
module Pos where

import Lens.Micro.TH (makeLenses)

data Pos = Pos { _posX :: !Int, _posY :: !Int }
  deriving (Eq, Show)

makeLenses ''Pos
