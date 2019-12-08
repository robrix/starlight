{-# LANGUAGE DeriveFunctor, DeriveGeneric #-}
module Control.Effect.Finalize
( -- * Finalize effect
  Finalize(..)
, finalize
  -- * Re-exports
, Algebra
, Has
, run
) where

import Control.Algebra
import GHC.Generics (Generic1)

data Finalize m k
  = Finalize (IO ()) (m k)
  deriving (Functor, Generic1)

instance HFunctor Finalize
instance Effect Finalize


finalize :: Has Finalize sig m => IO () -> m ()
finalize m = send (Finalize m (pure ()))
