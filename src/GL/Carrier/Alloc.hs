{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module GL.Carrier.Alloc
( -- * Alloc carrier
  AllocC(..)
  -- * Alloc effect
, module GL.Effect.Alloc
) where

import Control.Monad.IO.Class
import GL.Effect.Alloc

newtype AllocC m a = AllocC (m a)
  deriving (Applicative, Functor, Monad, MonadIO)
