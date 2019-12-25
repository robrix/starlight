{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module GL.Carrier.Bind
( -- * Bind carrier
  runBind
, BindC(..)
  -- * Bind effect
, module GL.Effect.Bind
) where

import Control.Carrier.Reader
import Control.Monad.IO.Class
import GL.Effect.Bind

runBind :: BindC t m a -> m a
runBind (BindC m) = runReader Nothing m

newtype BindC t m a = BindC (ReaderC (Maybe t) m a)
  deriving (Applicative, Functor, Monad, MonadIO)
