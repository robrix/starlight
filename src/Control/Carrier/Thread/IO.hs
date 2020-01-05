{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Control.Carrier.Thread.IO
( runThread
, ThreadC(ThreadC)
  -- * Thread effect
, module Control.Effect.Thread
) where

import Control.Effect.Thread
import Control.Monad.IO.Class

newtype ThreadC m a = ThreadC { runThread :: m a }
  deriving (Applicative, Functor, Monad, MonadIO)
