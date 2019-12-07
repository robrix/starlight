{-# LANGUAGE DeriveFunctor, ExistentialQuantification, StandaloneDeriving #-}
module GL.Effect.Alloc
( Alloc(..)
) where

import Control.Algebra
import GL.Object

data Alloc m k
  = forall t . Object t => Gen (t -> m k)

deriving instance Functor m => Functor (Alloc m)

instance HFunctor Alloc where
  hmap f (Gen k) = Gen (f . k)
