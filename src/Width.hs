module Width where

newtype Width = Width { unWidth :: Int }
  deriving (Eq, Ord, Show)
