{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE StandaloneDeriving #-}
module Control.Effect.Thread
( -- * Thread effect
  Thread(..)
) where

data Thread m k
  = forall a . Fork (m a) (a -> m k)

deriving instance Functor m => Functor (Thread m)
