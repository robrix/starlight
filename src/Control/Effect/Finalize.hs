{-# LANGUAGE DeriveFunctor, DeriveGeneric #-}
module Control.Effect.Finalize
( -- * Finalize effect
  Finalize(..)
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
