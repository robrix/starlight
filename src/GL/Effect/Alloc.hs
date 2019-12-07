{-# LANGUAGE DeriveFunctor, ExistentialQuantification, StandaloneDeriving #-}
module GL.Effect.Alloc
( -- * Alloc effect
  Alloc(..)
, genN
) where

import Control.Algebra
import GL.Object (Object)

data Alloc m k
  = forall t . Object t => Gen Int ([t] -> m k)

deriving instance Functor m => Functor (Alloc m)

instance HFunctor Alloc where
  hmap f (Gen n k) = Gen n (f . k)

instance Effect Alloc where
  thread ctx hdl (Gen n k) = Gen n (hdl . (<$ ctx) . k)

genN :: (Object t, Has Alloc sig m) => Int -> m [t]
genN n = send (Gen n pure)
