{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
module GL.Carrier.Check.Identity
( -- * Check carrier
  runCheck
, CheckC(CheckC)
  -- * Check effect
, module GL.Effect.Check
) where

import Control.Algebra
import Control.Monad.Fix
import Control.Monad.IO.Class
import GL.Effect.Check

newtype CheckC m a = CheckC { runCheck :: m a }
  deriving (Applicative, Functor, Monad, MonadFail, MonadFix, MonadIO)

instance Algebra sig m => Algebra (Check :+: sig) (CheckC m) where
  alg hdl = \case
    L (Check _) -> pure
    R other     -> CheckC . alg (runCheck . hdl) other
