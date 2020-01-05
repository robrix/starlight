{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module GL.Carrier.Check.Identity
( -- * Check carrier
  runCheck
, CheckC(CheckC)
  -- * Check effect
, module GL.Effect.Check
) where

import Control.Monad.IO.Class
import GL.Effect.Check

newtype CheckC m a = CheckC { runCheck :: m a }
  deriving (Applicative, Functor, Monad, MonadIO)
