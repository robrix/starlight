{-# LANGUAGE ExistentialQuantification #-}
module Control.Effect.Fork
( Fork(..)
) where

data Fork m k
  = forall a . Fork (m a) (a -> m k)
