{-# LANGUAGE DeriveFunctor #-}
module GL.Effect.Check
( -- * Check effect
  Check(..)
) where

import GHC.Stack

data Check m k
  = Check (Maybe (String, SrcLoc)) (m k)
  deriving (Functor)
