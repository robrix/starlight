{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Stochastic.PDF
( PDF(..)
) where

import Data.Semigroup (Sum(..))

newtype PDF a b = PDF { runPDF :: a -> b }
  deriving (Applicative, Functor, Monad)
  deriving (Monoid, Semigroup) via (a -> Sum b)
