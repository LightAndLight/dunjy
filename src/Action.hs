{-# options_ghc -fno-warn-unused-matches #-}
{-# language GADTs #-}
{-# language TemplateHaskell #-}
module Action where

import Data.Dependent.Map (DMap)
import Data.Foldable (toList)
import Data.Functor.Identity (Identity(..))
import Data.GADT.Compare.TH (deriveGEq, deriveGCompare)
import Data.GADT.Show.TH (deriveGShow)
import Data.List.NonEmpty (NonEmpty(..))
import System.Random (Random(..))

import qualified Data.Dependent.Map as DMap

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

data Move = Relative !Dir !Int | Absolute !Pos
  deriving (Eq, Show, Ord)

data Action a where
  Move :: !Dir -> Action Int
  MoveTo :: Action Pos
  Wait :: Action ()
  Melee :: Action Dir
deriveGEq ''Action
deriveGCompare ''Action
deriveGShow ''Action

moveAction :: DMap Action Identity -> Maybe (NonEmpty Move)
moveAction =
  DMap.foldrWithKey
    (\action (Identity a) acc ->
       case action of
         Move dir -> Just $ Relative dir a :| foldMap toList acc
         MoveTo -> Just $ Absolute a :| foldMap toList acc
         _ -> acc)
    Nothing
