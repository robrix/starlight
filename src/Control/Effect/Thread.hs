{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE StandaloneDeriving #-}
module Control.Effect.Thread
( -- * Thread effect
  Thread(..)
) where

import Control.Algebra

data Thread m k
  = forall a . Fork (m a) (a -> m k)

deriving instance Functor m => Functor (Thread m)

instance HFunctor Thread where
  hmap f (Fork m k) = Fork (f m) (f . k)
