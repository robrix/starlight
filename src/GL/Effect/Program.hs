{-# LANGUAGE DeriveFunctor #-}
module GL.Effect.Program
( -- * Program effect
  Program(..)
  -- * Re-exports
, Algebra
, Has
, run
) where

import Control.Algebra

data Program m k
  = Use (m k)
  deriving (Functor)
