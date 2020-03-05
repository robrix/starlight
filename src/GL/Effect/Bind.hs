{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE StandaloneDeriving #-}
module GL.Effect.Bind
( -- * Bind effect
  bind
, Bind(..)
  -- * Re-exports
, Algebra
, Effect
, Has
, run
) where

import Control.Algebra

bind :: Has (Bind t) sig m => t -> m a -> m a
bind t m = send (Bind t m pure)

data Bind t m k
  = forall a . Bind t (m a) (a -> m k)

deriving instance Functor m => Functor (Bind t m)

instance Effect (Bind t) where
  thread ctx hdl (Bind t m k) = Bind t (hdl (m <$ ctx)) (hdl . fmap k)
