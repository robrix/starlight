{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Control.Carrier.State.IORef
( -- * State carrier
  StateC(..)
) where

import Control.Carrier.Reader
import Control.Monad.IO.Class
import Data.IORef

newtype StateC s m a = StateC (ReaderC (IORef s) m a)
  deriving (Applicative, Functor, Monad, MonadIO)
