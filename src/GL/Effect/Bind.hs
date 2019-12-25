{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
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
import GHC.Generics (Generic)

data Bind a m k
  = Bind a (m k)
  deriving (Functor, Generic)
