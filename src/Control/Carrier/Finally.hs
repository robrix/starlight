{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Control.Carrier.Finally
( -- * Finally carrier
  FinallyC(..)
  -- * Finally effect
, module Control.Effect.Finally
) where

import Control.Effect.Finally
import Control.Monad.IO.Class

newtype FinallyC m a = FinallyC (m a)
  deriving (Applicative, Functor, Monad, MonadIO)
