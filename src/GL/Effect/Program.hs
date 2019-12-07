{-# LANGUAGE DeriveFunctor, DeriveGeneric #-}
module GL.Effect.Program
( -- * Program effect
  Program(..)
, use
  -- * Re-exports
, Algebra
, Has
, run
) where

import Control.Algebra
import GHC.Generics (Generic1)

data Program m k
  = Use (m k)
  deriving (Functor, Generic1)

instance HFunctor Program
instance Effect   Program


use :: Has Program sig m => m ()
use = send (Use (pure ()))
