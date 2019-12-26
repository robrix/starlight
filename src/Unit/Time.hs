{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module Unit.Time
( Seconds(..)
, fromDays
, fromHours
, fromMinutes
, Days(..)
) where

import Data.Proxy
import Foreign.Storable
import GL.Type as GL
import GL.Uniform

newtype Seconds a = Seconds { getSeconds :: a }
  deriving (Eq, Foldable, Floating, Fractional, Functor, Num, Ord, Real, RealFloat, RealFrac, Show, Storable, Traversable, Uniform)

instance GL.Type a => GL.Type (Seconds a) where
  glType _ = glType (Proxy @a)

  glDims _ = glDims (Proxy @a)

-- | Convert 'Days' to 'Seconds'. Note that this does not take e.g. leap seconds into account.
fromDays :: Num a => Days a -> Seconds a
fromDays (Days d) = Seconds (d * 86400)

fromHours :: Num a => a -> Seconds a
fromHours h = Seconds (h * 3600)

fromMinutes :: Num a => a -> Seconds a
fromMinutes h = Seconds (h * 60)


newtype Days a = Days { getDays :: a }
  deriving (Eq, Foldable, Floating, Fractional, Functor, Num, Ord, Real, RealFloat, RealFrac, Show, Storable, Traversable, Uniform)

instance GL.Type a => GL.Type (Days a) where
  glType _ = glType (Proxy @a)

  glDims _ = glDims (Proxy @a)
