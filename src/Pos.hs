{-# language TemplateHaskell #-}
module Pos where

import Reflex.Dynamic (Dynamic)

import Control.Lens.TH (makeLenses)
import System.Random (Random(..))

data Pos = Pos { _posX :: !Int, _posY :: !Int }
  deriving (Eq, Show, Ord)
makeLenses ''Pos

subtractPos :: Pos -> Pos -> Pos
subtractPos (Pos a b) (Pos c d) = Pos (c - a) (d - b)

instance Random Pos where
  randomR ((Pos alo blo), (Pos ahigh bhigh)) g =
    let
      (aval, g') = randomR (alo, ahigh) g
      (bval, g'') = randomR (blo, bhigh) g'
    in
      (Pos aval bval, g'')
  random g =
    let
      (aval, g') = random g
      (bval, g'') = random g'
    in
      (Pos aval bval, g'')


data Positioned t a = Positioned { _posThing :: a, _posPos :: Dynamic t Pos }

makeLenses ''Positioned
