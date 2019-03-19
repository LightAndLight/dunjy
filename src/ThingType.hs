module ThingType where

data ThingType = TPlayer | TThing !Int
  deriving (Eq, Ord, Show)
