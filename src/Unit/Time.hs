{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Unit.Time
( Seconds(..)
, fromDays
, fromHours
, fromMinutes
, module Unit
) where

import Data.Functor.Const
import Data.Functor.Identity
import Foreign.Storable
import GL.Type as GL
import GL.Uniform
import Linear.Metric
import Linear.Vector
import Unit

newtype Seconds a = Seconds { getSeconds :: a }
  deriving (Eq, Foldable, Floating, Fractional, Functor, Num, Ord, Real, RealFloat, RealFrac, Show, Storable, Traversable, GL.Type, Uniform)
  deriving (Additive, Applicative, Metric, Monad) via Identity

instance Unit Seconds where suffix = Const ('s':)

-- | Convert days to 'Seconds'. Note that this does not take e.g. leap seconds into account.
fromDays :: Num a => a -> Seconds a
fromDays d = fromHours (d * 24)

fromHours :: Num a => a -> Seconds a
fromHours h = fromMinutes (h * 60)

fromMinutes :: Num a => a -> Seconds a
fromMinutes m = Seconds (m * 60)
