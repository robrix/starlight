{-# LANGUAGE GADTs #-}
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

data Finally m k where
  OnExit :: m a -> Finally m ()


onExit :: Has Finally sig m => m () -> m ()
onExit m = send (OnExit m)
