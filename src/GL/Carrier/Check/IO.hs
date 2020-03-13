{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
module GL.Carrier.Check.IO
( -- * Check carrier
  runCheck
, CheckC(CheckC)
  -- * Check effect
, module GL.Effect.Check
) where

import Control.Algebra
import Control.Monad.Fix
import Control.Monad.IO.Class.Lift
import Data.Foldable (toList)
import GHC.Stack
import GL.Effect.Check
import GL.Error
import Graphics.GL.Core41

newtype CheckC m a = CheckC { runCheck :: m a }
  deriving (Applicative, Functor, Monad, MonadFail, MonadFix, MonadIO)

instance Has (Lift IO) sig m => Algebra (Check :+: sig) (CheckC m) where
  alg hdl sig ctx = case sig of
    L (Check loc k) -> do
      err <- runLiftIO glGetError
      case err of
        GL_NO_ERROR -> hdl (k <$ ctx)
        other       -> withCallStack (fromCallSiteList (toList loc)) (withFrozenCallStack (throwGLError other)) >> hdl (k <$ ctx)
    R other -> CheckC (alg (runCheck . hdl) other ctx)

withCallStack :: CallStack -> (HasCallStack => a) -> a
withCallStack callStack a = let ?callStack = callStack in a
