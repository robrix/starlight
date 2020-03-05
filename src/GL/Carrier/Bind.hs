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
  alg ctx hdl = \case
    L (Bind t m k) -> do
      prev <- BindC ask
      GL.bind (Just t)
      a <- BindC (local (const (Just t)) (runBindC (hdl (m <$ ctx))))
      GL.bind (prev `asTypeOf` Just t)
      hdl (fmap k a)
    R other -> BindC (alg ctx (runBindC . hdl) (R other))
