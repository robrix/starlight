{-# LANGUAGE DeriveFunctor, ExistentialQuantification, StandaloneDeriving #-}
module Control.Effect.Profile
( -- * Profile effect
  Profile(..)
  -- * Re-exports
, Algebra
, Effect
, Has
, run
) where

import Control.Algebra

data Profile m k
  = forall a . Measure String (m a) (a -> m k)

deriving instance Functor m => Functor (Profile m)
