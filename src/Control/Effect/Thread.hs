{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StandaloneDeriving #-}
module Control.Effect.Thread
( -- * Thread effect
  fork
, yield
, Thread(..)
) where

import Control.Algebra

fork :: Has Thread sig m => m () -> m ()
fork m = send (Fork m (pure ()))

yield :: Has Thread sig m => m ()
yield = send (Yield (pure ()))

data Thread m k
  = forall a . Fork (m a) (m k)
  | Yield (m k)

deriving instance Functor m => Functor (Thread m)

instance HFunctor Thread where
  hmap f = \case
    Fork m k -> Fork (f m) (f k)
    Yield  k -> Yield      (f k)

instance Effect Thread where
  thread ctx hdl = \case
    Fork m k -> Fork (hdl (m <$ ctx)) (hdl (k <$ ctx))
    Yield  k -> Yield                 (hdl (k <$ ctx))
