module Action where

import Pos

data Dir = L | R | U | D
  deriving (Eq, Show)

data Action
  = Move !Dir !Int
  | MoveTo !Pos
  | Wait
  deriving (Eq, Show)