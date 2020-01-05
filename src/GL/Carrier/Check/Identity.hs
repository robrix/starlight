{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
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
import Control.Monad.IO.Class
import GL.Effect.Check

newtype CheckC m a = CheckC { runCheck :: m a }
  deriving (Applicative, Functor, Monad, MonadIO)

instance Algebra sig m => Algebra (Check :+: sig) (CheckC m) where
  alg = \case
    L (Check _ k) -> k
    R other -> CheckC (send (handleCoercible other))
