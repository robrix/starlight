{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module GL.Carrier.Bind
( -- * Bind carrier
  BindC(..)
  -- * Bind effect
, module GL.Effect.Bind
) where

import Control.Carrier.Reader
import Control.Monad.IO.Class
import GL.Effect.Bind

newtype BindC t m a = BindC (ReaderC t m a)
  deriving (Applicative, Functor, Monad, MonadIO)
