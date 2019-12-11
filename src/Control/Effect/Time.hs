{-# LANGUAGE DeriveFunctor, DeriveGeneric #-}
module Control.Effect.Time
( Time(..)
) where

import Control.Algebra
import Data.Time.Clock
import GHC.Generics (Generic1)

data Time m k
  = Now (UTCTime -> m k)
  deriving (Functor, Generic1)

instance HFunctor Time
instance Effect Time
