module Unit.Count
( Count(..)
) where

newtype Count a = Count { getCount :: a }
