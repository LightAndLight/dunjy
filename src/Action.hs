module Action where

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

instance Random Dir where
  randomR (lo, hi) g = (toEnum a, g')
    where
      (a, g') = randomR (fromEnum lo, fromEnum hi) g

  random = randomR (minBound, maxBound)

data Action
  = Move !Dir !Int
  | MoveTo !Pos
  | Wait
  | Melee !Dir
  deriving (Eq, Show)
