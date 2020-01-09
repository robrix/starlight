{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Unit.Time
( Seconds(..)
, getSeconds
, fromDays
, fromHours
, fromMinutes
, module Unit
) where

import Foreign.Storable
import GL.Type as GL
import GL.Uniform
import Unit

newtype Seconds a = Seconds a
  deriving (Eq, Foldable, Floating, Fractional, Functor, Num, Ord, Read, Real, RealFloat, RealFrac, Show, Storable, Traversable, GL.Type, Uniform)

instance Unit Seconds

getSeconds :: Seconds a -> a
getSeconds (Seconds a) = a

-- | Convert days to 'Seconds'. Note that this does not take e.g. leap seconds into account.
fromDays :: Num a => a -> Seconds a
fromDays d = fromHours (d * 24)

fromHours :: Num a => a -> Seconds a
fromHours h = fromMinutes (h * 60)

fromMinutes :: Num a => a -> Seconds a
fromMinutes m = Seconds (m * 60)
