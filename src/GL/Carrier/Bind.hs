{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
module GL.Carrier.Bind
( -- * Bind carrier
  runBind
, BindC(BindC)
  -- * Bind effect
, module GL.Effect.Bind
) where

import Control.Algebra
import Control.Effect.Lift
import Control.Carrier.Reader
import Control.Monad.IO.Class
import GL.Effect.Bind
import qualified GL.Object as GL

runBind :: BindC t m a -> m a
runBind = runReader Nothing . runBindC

newtype BindC t m a = BindC { runBindC :: ReaderC (Maybe t) m a }
  deriving (Applicative, Functor, Monad, MonadIO)

instance (Has (Lift IO) sig m, GL.Bind t) => Algebra (Bind t :+: sig) (BindC t m) where
  alg = \case
    L (Bind t m k) -> do
      prev <- BindC ask
      GL.bind (Just t)
      a <- BindC (local (const (Just t)) (runBindC m))
      GL.bind (prev `asTypeOf` Just t)
      k a
    R other -> BindC (send (handleCoercible other))
