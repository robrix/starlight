{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Stochastic.PDF
( PDF(..)
) where

import Data.Semigroup (Sum(..))

newtype PDF a b = PDF { runPDF :: a -> b }
  deriving (Applicative, Functor, Monad)
  deriving Semigroup via (a -> Sum b)
