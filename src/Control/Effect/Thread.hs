{-# LANGUAGE ExistentialQuantification #-}
module Control.Effect.Thread
( -- * Thread effect
  Thread(..)
) where

data Thread m k
  = forall a . Fork (m a) (a -> m k)
