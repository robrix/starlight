{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Control.Carrier.Finally
( -- * Finally carrier
  FinallyC(..)
  -- * Finally effect
, module Control.Effect.Finally
) where

import Control.Carrier.State.IORef
import Control.Effect.Finally
import Control.Monad.IO.Class

newtype FinallyC m a = FinallyC (StateC [m ()] m a)
  deriving (Applicative, Functor, Monad, MonadIO)
