{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
module GL.Effect.Check
( -- * Check effect
  check
, checking
, Check(..)
  -- * Re-export
, Algebra
, Has
, run
) where

import Control.Algebra
import Data.Kind (Type)
import Data.Maybe (listToMaybe)
import GHC.Stack

check :: (Has Check sig m, HasCallStack) => m ()
check = send (Check (listToMaybe (getCallStack callStack)))

checking :: (Has Check sig m, HasCallStack) => m a -> m a
checking action = withFrozenCallStack $ action <* check

data Check (m :: Type -> Type) k where
  Check :: Maybe (String, SrcLoc) -> Check m ()
