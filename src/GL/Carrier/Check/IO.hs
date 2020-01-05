{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module GL.Carrier.Check.IO
( -- * Check carrier
  runCheck
, CheckC(CheckC)
) where

import Control.Monad.IO.Class

newtype CheckC m a = CheckC { runCheck :: m a }
  deriving (Applicative, Functor, Monad, MonadIO)
