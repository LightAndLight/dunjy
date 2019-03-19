{-# options_ghc -fno-warn-unused-matches #-}
{-# language GADTs #-}
{-# language TemplateHaskell #-}
module Action where

import Data.GADT.Compare.TH (deriveGEq, deriveGCompare)
import Data.GADT.Show.TH (deriveGShow)
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

data Action a where
  Move :: !Dir -> Action Int
  MoveTo :: Action Pos
  Wait :: Action ()
  Melee :: Action Dir
deriveGEq ''Action
deriveGCompare ''Action
deriveGShow ''Action
