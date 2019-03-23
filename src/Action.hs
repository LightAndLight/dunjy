{-# language TemplateHaskell #-}
module Action where

import Control.Lens.TH (makePrisms)
import System.Random (Random(..))

import Pos

data Dir
  = L
  | UL
  | U
  | UR
  | R
  | DR
  | D
  | DL
  deriving (Eq, Show, Ord, Enum, Bounded)

reverseDir :: Dir -> Dir
reverseDir L = R
reverseDir UL = DR
reverseDir U = D
reverseDir UR = DL
reverseDir R = L
reverseDir DR = UL
reverseDir D = U
reverseDir DL = UR

instance Random Dir where
  randomR (lo, hi) g = (toEnum a, g')
    where
      (a, g') = randomR (fromEnum lo, fromEnum hi) g

  random = randomR (minBound, maxBound)

data Move
  = Relative !Dir
  | Absolute !Pos
  deriving (Eq, Show, Ord)
makePrisms ''Move

data Action
  = Move !Move
  | Wait
  | Melee !Dir
  deriving (Eq, Show, Ord)
makePrisms ''Action
