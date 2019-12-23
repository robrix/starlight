module Data.Interval
( Interval(..)
) where

data Interval a = Interval
  { from :: !a
  , to   :: !a
  }
