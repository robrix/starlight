{-# LANGUAGE FlexibleInstances, GeneralizedNewtypeDeriving, LambdaCase, MultiParamTypeClasses, TypeOperators, UndecidableInstances #-}
module GL.Carrier.Alloc
( -- * Alloc carrier
  runAlloc
, AllocC(..)
  -- * Alloc effect
, module GL.Effect.Alloc
) where

import Control.Algebra
import Control.Carrier.Lift
import Control.Monad.IO.Class
import GL.Effect.Alloc
import qualified GL.Object as GL

runAlloc :: AllocC m a -> m a
runAlloc (AllocC m) = m

newtype AllocC m a = AllocC (m a)
  deriving (Applicative, Functor, Monad, MonadIO)

instance Has (Lift IO) sig m => Algebra (Alloc :+: sig) (AllocC m) where
  alg = \case
    L (Gen n k) -> GL.withN n k
    R other     -> AllocC (send (handleCoercible other))
