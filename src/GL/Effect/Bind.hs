module GL.Effect.Bind
( -- * Bind effect
  Bind(..)
  -- * Re-exports
, Algebra
, Effect
, Has
, run
) where

import Control.Algebra

data Bind a m k
  = Bind a (m k)
