{-# LANGUAGE DeriveFunctor, ExistentialQuantification, StandaloneDeriving #-}
module GL.Effect.Alloc
( Alloc(..)
) where

import GL.Object

data Alloc m k
  = forall t . Object t => Gen (t -> m k)

deriving instance Functor m => Functor (Alloc m)
