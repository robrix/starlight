{-# LANGUAGE DeriveFunctor, ExistentialQuantification, StandaloneDeriving #-}
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

data Finally m k
  = forall a . Finally (m a) (m k)

deriving instance Functor m => Functor (Finally m)

instance HFunctor Finally where
  hmap f (Finally m k) = Finally (f m) (f k)

instance Effect Finally where
  thread ctx hdl (Finally m k) = Finally (hdl (m <$ ctx)) (hdl (k <$ ctx))


finally :: Has Finally sig m => m () -> m ()
finally m = send (Finally m (pure ()))
