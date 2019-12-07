{-# LANGUAGE DeriveFunctor, ExistentialQuantification, StandaloneDeriving #-}
module GL.Effect.Alloc
( -- * Alloc effect
  Alloc(..)
, gen
) where

import Control.Algebra
import GL.Object (Object)

data Alloc m k
  = forall t . Object t => Gen (t -> m k)

deriving instance Functor m => Functor (Alloc m)

instance HFunctor Alloc where
  hmap f (Gen k) = Gen (f . k)

instance Effect Alloc where
  thread ctx hdl (Gen k) = Gen (hdl . (<$ ctx) . k)

gen :: (Object t, Has Alloc sig m) => m t
gen = send (Gen pure)
