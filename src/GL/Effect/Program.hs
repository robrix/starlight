{-# LANGUAGE DeriveFunctor, ExistentialQuantification, LambdaCase, StandaloneDeriving #-}
module GL.Effect.Program
( -- * Program effect
  Program(..)
, build
, use
, set
  -- * Re-exports
, Algebra
, Has
, run
) where

import Control.Algebra
import GHC.TypeLits
import qualified GL.Program as GL
import GL.Shader
import GL.Uniform

data Program m k
  = forall ty . Build [(ShaderType, FilePath)] (GL.Program ty -> m k)
  | forall ty . Use (GL.Program ty) (m k)
  | forall name a ty . (KnownSymbol name, Uniform a) => Set (GL.Program ty) (Var name a) a (m k)

deriving instance Functor m => Functor (Program m)

instance HFunctor Program where
  hmap f = \case
    Build s   k -> Build s   (f . k)
    Use p     k -> Use p     (f k)
    Set p v a k -> Set p v a (f k)

instance Effect   Program where
  thread ctx hdl = \case
    Build s   k -> Build s   (hdl . (<$ ctx) . k)
    Use p     k -> Use p     (hdl (k <$ ctx))
    Set p v a k -> Set p v a (hdl (k <$ ctx))


build :: Has Program sig m => [(ShaderType, FilePath)] -> m (GL.Program ty)
build s = send (Build s pure)

use :: Has Program sig m => (GL.Program ty) -> m ()
use p = send (Use p (pure ()))

set :: (KnownSymbol name, Uniform a, Has Program sig m) => (GL.Program ty) -> Var name a -> a -> m ()
set p v a = send (Set p v a (pure ()))
