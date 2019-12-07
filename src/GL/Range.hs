module GL.Range
( Range(..)
) where

data Range = Range
  { from :: {-# UNPACK #-} !Int
  , size :: {-# UNPACK #-} !Int
  }
  deriving (Eq, Show)
