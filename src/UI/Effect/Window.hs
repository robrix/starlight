{-# LANGUAGE DeriveFunctor, ExistentialQuantification, LambdaCase, StandaloneDeriving #-}
module UI.Effect.Window
( -- * Window effect
  Window(..)
, loop
, stop
, size
, scale
  -- * Re-exports
, Algebra
, Has
, run
) where

import Control.Algebra
import Linear.V2

data Window m k
  = forall a . Loop (m a) (a -> m k)
  | Stop (m k)
  | Size (V2 Integer -> m k)
  | Scale (Integer -> m k)

deriving instance Functor m => Functor (Window m)

instance HFunctor Window where
  hmap f = \case
    Loop m k -> Loop (f m) (f . k)
    Stop   k -> Stop       (f k)
    Size   k -> Size       (f . k)
    Scale  k -> Scale      (f . k)

instance Effect Window where
  thread ctx hdl = \case
    Loop m k -> Loop (hdl (m <$ ctx)) (hdl . fmap k)
    Stop   k -> Stop                  (hdl (k <$ ctx))
    Size   k -> Size                  (hdl . (<$ ctx) . k)
    Scale  k -> Scale                 (hdl . (<$ ctx) . k)

loop :: Has Window sig m => m a -> m a
loop m = send (Loop m pure)

stop :: Has Window sig m => m ()
stop = send (Stop (pure ()))

size :: (Num a, Has Window sig m) => m (V2 a)
size = send (Size (pure . fmap fromInteger))

scale :: (Num a, Has Window sig m) => m a
scale = send (Scale (pure . fromInteger))
