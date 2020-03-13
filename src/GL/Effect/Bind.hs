{-# LANGUAGE GADTs #-}
module GL.Effect.Bind
( -- * Bind effect
  bind
, Bind(..)
  -- * Re-exports
, Algebra
, Has
, run
) where

import Control.Algebra

bind :: Has (Bind t) sig m => t -> m a -> m a
bind t m = send (Bind t m)

data Bind t m k where
  Bind :: t -> m a -> Bind t m a
