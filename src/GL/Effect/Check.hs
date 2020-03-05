{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
module GL.Effect.Check
( -- * Check effect
  check
, checking
, Check(..)
  -- * Re-export
, Algebra
, Effect
, Has
, run
) where

import Control.Algebra
import Data.Maybe (listToMaybe)
import GHC.Generics (Generic1)
import GHC.Stack

check :: (Has Check sig m, HasCallStack) => m ()
check = send (Check (listToMaybe (getCallStack callStack)) (pure ()))

checking :: (Has Check sig m, HasCallStack) => m a -> m a
checking action = withFrozenCallStack $ action <* check

data Check m k
  = Check (Maybe (String, SrcLoc)) (m k)
  deriving (Functor, Generic1)

instance Effect Check
