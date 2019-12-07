{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module GL.Carrier.Alloc
( -- * Alloc carrier
  runAlloc
, AllocC(..)
  -- * Alloc effect
, module GL.Effect.Alloc
) where

import Control.Monad.IO.Class
import GL.Effect.Alloc

runAlloc :: AllocC m a -> m a
runAlloc (AllocC m) = m

newtype AllocC m a = AllocC (m a)
  deriving (Applicative, Functor, Monad, MonadIO)
