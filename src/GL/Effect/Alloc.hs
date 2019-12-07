{-# LANGUAGE ExistentialQuantification #-}
module GL.Effect.Alloc
( Alloc(..)
) where

import GL.Object

data Alloc m k
  = forall t . Object t => Gen (t -> m k)
