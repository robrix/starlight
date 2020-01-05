{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
module GL.Effect.Check
( -- * Check effect
  check
, Check(..)
) where

import Control.Algebra
import Data.Maybe (listToMaybe)
import GHC.Generics (Generic1)
import GHC.Stack

check :: (Has Check sig m, HasCallStack) => m ()
check = send (Check (listToMaybe (getCallStack callStack)) (pure ()))

data Check m k
  = Check (Maybe (String, SrcLoc)) (m k)
  deriving (Functor, Generic1)

instance HFunctor Check
instance Effect Check
