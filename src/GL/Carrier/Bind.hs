{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
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

import           Control.Algebra
import           Control.Carrier.Reader
import           Control.Effect.Lift
import           Control.Monad.IO.Class
import           GL.Effect.Bind
import           GL.Effect.Check
import qualified GL.Object as GL

runBind :: BindC t m a -> m a
runBind = runReader Nothing . runBindC

newtype BindC t m a = BindC { runBindC :: ReaderC (Maybe t) m a }
  deriving (Applicative, Functor, Monad, MonadIO)

instance (Has Check sig m, Has (Lift IO) sig m, GL.Bind t) => Algebra (Bind t :+: sig) (BindC t m) where
  alg hdl sig ctx = case sig of
    L (Bind t m) -> do
      prev <- BindC ask
      GL.bind (Just t)
      a <- BindC (local (const (Just t)) (runBindC (hdl (m <$ ctx))))
      a <$ GL.bind (prev `asTypeOf` Just t)
    R other -> BindC (alg (runBindC . hdl) (R other) ctx)
