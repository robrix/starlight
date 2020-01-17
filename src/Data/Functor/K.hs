{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- | I got sick of writing 'Const' out in full.
module Data.Functor.K
( K(..)
) where

import Data.Functor.Const
import Foreign.Storable
import Linear

newtype K a b = K { getK :: a }
  deriving (Conjugate, Epsilon, Enum, Eq, Foldable, Floating, Fractional, Functor, Integral, Num, Ord, Real, RealFloat, RealFrac, Show, Storable, Traversable)
  deriving (Applicative) via Const a
