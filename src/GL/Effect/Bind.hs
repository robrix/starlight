{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE StandaloneDeriving #-}
module GL.Effect.Bind
( -- * Bind effect
  Bind(..)
  -- * Re-exports
, Algebra
, Effect
, Has
, run
) where

import Control.Algebra

data Bind t m k
  = forall a . Bind t (m a) (a -> m k)

deriving instance Functor m => Functor (Bind t m)
