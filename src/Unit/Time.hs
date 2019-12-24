{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Unit.Time
( Seconds(..)
, fromDays
, Days(..)
) where

import GL.Uniform

newtype Seconds a = Seconds { getSeconds :: a }
  deriving (Eq, Foldable, Floating, Fractional, Functor, Num, Ord, Real, RealFloat, RealFrac, Show, Traversable, Uniform)

-- | Convert 'Days' to 'Seconds'. Note that this does not take e.g. leap seconds into account.
fromDays :: Num a => Days a -> Seconds a
fromDays (Days d) = Seconds (d * 86400)


newtype Days a = Days { getDays :: a }
  deriving (Eq, Foldable, Floating, Fractional, Functor, Num, Ord, Real, RealFloat, RealFrac, Show, Traversable, Uniform)
