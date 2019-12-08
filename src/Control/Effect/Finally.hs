{-# LANGUAGE DeriveFunctor, DeriveGeneric #-}
module Control.Effect.Finally
( -- * Finally effect
  Finally(..)
, finally
  -- * Re-exports
, Algebra
, Has
, run
) where

import Control.Algebra
import GHC.Generics (Generic1)

data Finally m k
  = Finally (IO ()) (m k)
  deriving (Functor, Generic1)

instance HFunctor Finally
instance Effect Finally


finally :: Has Finally sig m => IO () -> m ()
finally m = send (Finally m (pure ()))
