{-# LANGUAGE DeriveFunctor, ExistentialQuantification, LambdaCase, StandaloneDeriving #-}
module UI.Effect.Window
( -- * Window effect
  Window(..)
, draw
, size
  -- * Re-exports
, Algebra
, Has
, run
) where

import Control.Algebra
import Linear.V2

data Window m k
  = forall a . Draw (m a) (a -> m k)
  | Size (V2 Int -> m k)

deriving instance Functor m => Functor (Window m)

instance HFunctor Window where
  hmap f = \case
    Draw m k -> Draw (f m) (f . k)
    Size   k -> Size       (f . k)

instance Effect Window where
  thread ctx hdl = \case
    Draw m k -> Draw (hdl (m <$ ctx)) (hdl . fmap k)
    Size   k -> Size                  (hdl . (<$ ctx) . k)

draw :: Has Window sig m => m a -> m a
draw m = send (Draw m pure)

size :: Has Window sig m => m (V2 Int)
size = send (Size pure)
