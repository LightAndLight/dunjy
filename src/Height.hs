module Height where

newtype Height = Height { unHeight :: Int }
  deriving (Eq, Ord, Show)