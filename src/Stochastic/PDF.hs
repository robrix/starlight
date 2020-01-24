{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Stochastic.PDF
( PDF(..)
) where

import Control.Applicative (liftA2)
import Data.Semigroup (Sum(..))

newtype PDF a b = PDF { runPDF :: a -> b }
  deriving (Applicative, Functor, Monad)
  deriving (Monoid, Semigroup) via (a -> Sum b)

instance Num b => Num (PDF a b) where
  (+) = liftA2 (+)
  (-) = liftA2 (-)
  (*) = liftA2 (*)
  negate = fmap negate
  abs    = fmap abs
  signum = fmap signum
  fromInteger = pure . fromInteger

instance Fractional b => Fractional (PDF a b) where
  (/) = liftA2 (/)
  recip = fmap recip
  fromRational = pure . fromRational
