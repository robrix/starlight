{-# LANGUAGE ExistentialQuantification #-}
module Control.Effect.Profile
( Profile(..)
) where

data Profile m k
  = forall a . Measure String (m a) (a -> m k)
