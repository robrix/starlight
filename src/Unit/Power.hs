module Unit.Power
( Watts(..)
, getWatts
, module Unit
) where

import Unit

newtype Watts a = Watts a

getWatts :: Watts a -> a
getWatts (Watts a) = a
