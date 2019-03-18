module Action where

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
  deriving (Eq, Show)

data Action
  = Move !Dir !Int
  | MoveTo !Pos
  | Wait
  deriving (Eq, Show)
