{-# LANGUAGE AllowAmbiguousTypes, DeriveFunctor, ExistentialQuantification, LambdaCase, PolyKinds, ScopedTypeVariables, StandaloneDeriving, TypeApplications #-}
module GL.Effect.Program
( -- * Program effect
  Program(..)
, use
  -- * Re-exports
, Algebra
, Has
, run
) where

import Control.Algebra
import GL.Uniform

data Program name m k
  = Use (m k)
  | forall a . Uniform a => Set (Var a) a (m k)

deriving instance Functor m => Functor (Program name m)

instance HFunctor (Program name) where
  hmap f = \case
    Use k     -> Use (f k)
    Set v a k -> Set v a (f k)

instance Effect   (Program name) where
  thread ctx hdl = \case
    Use k     -> Use     (hdl (k <$ ctx))
    Set v a k -> Set v a (hdl (k <$ ctx))


use :: forall name sig m . Has (Program name) sig m => m ()
use = send (Use @name (pure ()))
