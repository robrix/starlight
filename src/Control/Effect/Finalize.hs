module Control.Effect.Finalize
( -- * Finalize effect
  Finalize(..)
  -- * Re-exports
, Algebra
, Has
, run
) where

import Control.Algebra

data Finalize m k
  = Finalize (IO ()) (m k)
