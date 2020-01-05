{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Control.Carrier.Thread.IO
( ThreadC(..)
) where

import Control.Monad.IO.Class

newtype ThreadC m a = ThreadC (m a)
  deriving (Applicative, Functor, Monad, MonadIO)
