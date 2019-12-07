module GL.Carrier.Alloc
( -- * Alloc carrier
  AllocC(..)
  -- * Alloc effect
, module GL.Effect.Alloc
) where

import GL.Effect.Alloc

newtype AllocC m a = AllocC (m a)
