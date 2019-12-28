{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module Unit.Time
( Seconds(..)
, getSeconds
, fromDays
, fromHours
, fromMinutes
) where

import Data.Proxy
import Foreign.Storable
import GL.Type as GL
import GL.Uniform

newtype Seconds a = Seconds a
  deriving (Eq, Foldable, Floating, Fractional, Functor, Num, Ord, Read, Real, RealFloat, RealFrac, Show, Storable, Traversable, Uniform)

instance GL.Type a => GL.Type (Seconds a) where
  glType _ = glType (Proxy @a)

  glDims _ = glDims (Proxy @a)

getSeconds :: Seconds a -> a
getSeconds (Seconds a) = a

-- | Convert days to 'Seconds'. Note that this does not take e.g. leap seconds into account.
fromDays :: Num a => a -> Seconds a
fromDays d = fromHours (d * 24)

fromHours :: Num a => a -> Seconds a
fromHours h = fromMinutes (h * 60)

fromMinutes :: Num a => a -> Seconds a
fromMinutes m = Seconds (m * 60)
