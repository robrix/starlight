{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE LambdaCase #-}
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
import Control.Monad.IO.Class.Lift
import Data.Foldable (toList)
import GHC.Stack
import GL.Effect.Check
import GL.Error
import Graphics.GL.Core41

newtype CheckC m a = CheckC { runCheck :: m a }
  deriving (Applicative, Functor, Monad, MonadIO)

instance Has (Lift IO) sig m => Algebra (Check :+: sig) (CheckC m) where
  alg = \case
    L (Check loc k) -> do
      err <- runLiftIO glGetError
      case err of
        GL_NO_ERROR -> k
        other       -> withCallStack (fromCallSiteList (toList loc)) (withFrozenCallStack (throwGLError other)) >> k
    R other -> CheckC (send (handleCoercible other))

withCallStack :: CallStack -> (HasCallStack => a) -> a
withCallStack callStack a = let ?callStack = callStack in a
