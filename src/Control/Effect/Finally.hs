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


onExit :: Has Finally sig m => m () -> m ()
onExit m = send (OnExit m (pure ()))
