{-# LANGUAGE DataKinds, DeriveFunctor, ExistentialQuantification, ExplicitForAll, FlexibleInstances, FunctionalDependencies, GeneralizedNewtypeDeriving, KindSignatures, LambdaCase, StandaloneDeriving, TypeOperators, UndecidableInstances #-}
module GL.Effect.Program
( -- * Program effect
  Program(..)
, build
, use
, set
, ProgramT(..)
  -- * Re-exports
, (GL.:::)(..)
, GL.Var(..)
, Algebra
, Has
, run
) where

import Control.Algebra
import Control.Monad.IO.Class
import GHC.TypeLits
import qualified GL.Program as GL
import GL.Shader

data Program m k
  = forall ty . Build [(ShaderType, FilePath)] (GL.Program ty -> m k)
  | forall ty a . Use (GL.Program ty) (m a) (a -> m k)
  | forall name a ty . GL.HasUniform name a ty => Set (GL.Program ty) (GL.Var name a) (m k)

deriving instance Functor m => Functor (Program m)

instance HFunctor Program where
  hmap f = \case
    Build s k -> Build s     (f . k)
    Use p m k -> Use p (f m) (f . k)
    Set p v k -> Set p v     (f k)

instance Effect   Program where
  thread ctx hdl = \case
    Build s k -> Build s                (hdl . (<$ ctx) . k)
    Use p m k -> Use p (hdl (m <$ ctx)) (hdl . fmap k)
    Set p v k -> Set p v                (hdl (k <$ ctx))


build :: forall ty m sig . Has Program sig m => [(ShaderType, FilePath)] -> m (GL.Program ty)
build s = send (Build s pure)

use :: Has Program sig m => GL.Program ty -> ProgramT ty m a -> m a
use p (ProgramT m) = send (Use p m pure)

set :: (GL.HasUniform name a ty, Has Program sig m) => GL.Program ty -> GL.Var name a -> m ()
set p v = send (Set p v (pure ()))


class HasProgram (ty :: [Symbol GL.::: *]) (m :: * -> *) | m -> ty


newtype ProgramT (ty :: [Symbol GL.::: *]) m a = ProgramT (m a)
  deriving (Applicative, Functor, Monad, MonadIO)

instance Algebra sig m => Algebra sig (ProgramT ty m) where
  alg = ProgramT . send . handleCoercible

instance HasProgram ty (ProgramT ty m)
