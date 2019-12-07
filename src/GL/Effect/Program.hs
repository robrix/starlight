{-# LANGUAGE AllowAmbiguousTypes, DeriveFunctor, DeriveGeneric, PolyKinds, ScopedTypeVariables, TypeApplications #-}
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

data Program name m k
  = Use (m k)
  deriving (Functor, Generic1)

instance HFunctor (Program name)
instance Effect   (Program name)


use :: forall name sig m . Has (Program name) sig m => m ()
use = send (Use @name (pure ()))
