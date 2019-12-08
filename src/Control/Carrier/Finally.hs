module Control.Carrier.Finally
( -- * Finally carrier
  FinallyC(..)
  -- * Finally effect
, module Control.Effect.Finally
) where

import Control.Effect.Finally

newtype FinallyC m a = FinallyC (m a)
