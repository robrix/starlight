{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Control.Carrier.State.STM.TVar
( runStateVar
, StateC(..)
) where

import Control.Carrier.Reader
import Control.Concurrent.STM.TVar
import Control.Monad.IO.Class
import Control.Monad.Trans.Class

runStateVar :: TVar s -> StateC s m a -> m a
runStateVar var (StateC m) = runReader var m

newtype StateC s m a = StateC (ReaderC (TVar s) m a)
  deriving (Applicative, Functor, Monad, MonadFail, MonadIO, MonadTrans)
