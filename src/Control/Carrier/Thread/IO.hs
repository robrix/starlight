{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Control.Carrier.Thread.IO
( runThread
, ThreadC(ThreadC)
  -- * Thread effect
, module Control.Effect.Thread
) where

import Control.Monad.IO.Class
import Control.Effect.Thread

newtype ThreadC m a = ThreadC { runThread :: m a }
  deriving (Applicative, Functor, Monad, MonadIO)
