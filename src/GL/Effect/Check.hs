{-# LANGUAGE DeriveFunctor, DeriveGeneric #-}
module GL.Effect.Check
( -- * Check effect
  Check(..)
) where

import Control.Algebra
import GHC.Generics (Generic1)
import GHC.Stack

data Check m k
  = Check (Maybe (String, SrcLoc)) (m k)
  deriving (Functor, Generic1)

instance HFunctor Check
