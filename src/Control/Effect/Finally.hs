{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE StandaloneDeriving #-}
module Control.Effect.Finally
( -- * Finally effect
  Finally(..)
, onExit
  -- * Re-exports
, Algebra
, Has
, run
) where

import Control.Algebra

data Finally m k
  = forall a . OnExit (m a) (m k)

deriving instance Functor m => Functor (Finally m)

instance HFunctor Finally where
  hmap f (OnExit m k) = OnExit (f m) (f k)

instance Effect Finally where
  thread ctx hdl (OnExit m k) = OnExit (hdl (m <$ ctx)) (hdl (k <$ ctx))


onExit :: Has Finally sig m => m () -> m ()
onExit m = send (OnExit m (pure ()))
