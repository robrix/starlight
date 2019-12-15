{-# LANGUAGE DeriveFunctor, DeriveGeneric #-}
module UI.Effect.Window
( -- * Window effect
  Window(..)
, swap
, poll
, size
, scale
  -- * Re-exports
, Algebra
, Has
, run
) where

import Control.Algebra
import GHC.Generics (Generic1)
import Linear.V2
import qualified SDL

data Window m k
  = Swap (m k)
  | Poll (Maybe SDL.Event -> m k)
  | Size (V2 Integer -> m k)
  | Scale (Integer -> m k)
  deriving (Functor, Generic1)

instance HFunctor Window
instance Effect Window

swap :: Has Window sig m => m ()
swap = send (Swap (pure ()))

poll :: Has Window sig m => m (Maybe SDL.Event)
poll = send (Poll pure)

size :: (Num a, Has Window sig m) => m (V2 a)
size = send (Size (pure . fmap fromInteger))

scale :: (Num a, Has Window sig m) => m a
scale = send (Scale (pure . fromInteger))
