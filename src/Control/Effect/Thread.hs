{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE StandaloneDeriving #-}
module Control.Effect.Thread
( -- * Thread effect
  Thread(..)
) where

import Control.Algebra

data Thread m k
  = forall a . Fork (m a) (m k)

deriving instance Functor m => Functor (Thread m)

instance HFunctor Thread where
  hmap f (Fork m k) = Fork (f m) (f k)

instance Effect Thread where
  thread ctx hdl (Fork m k) = Fork (hdl (m <$ ctx)) (hdl (k <$ ctx))
