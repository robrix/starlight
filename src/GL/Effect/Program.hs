{-# LANGUAGE AllowAmbiguousTypes, DataKinds, DeriveFunctor, ExistentialQuantification, FlexibleInstances, FunctionalDependencies, GeneralizedNewtypeDeriving, KindSignatures, LambdaCase, ScopedTypeVariables, StandaloneDeriving, TypeApplications, TypeOperators, UndecidableInstances #-}
module GL.Effect.Program
( -- * Program effect
  Program(..)
, build
, use
, set
, HasProgram(..)
, ProgramT(..)
  -- * Re-exports
, (:::)(..)
, GL.Var(..)
, Algebra
, Has
, run
) where

import Control.Algebra
import Control.Carrier.Reader
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Data.DSL
import qualified GL.Program as GL
import GL.Shader as Shader

data Program m k
  = forall ty . Build [(Shader.Type, FilePath)] (GL.Program ty -> m k)
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


build :: forall ty m sig . Has Program sig m => [(Shader.Type, FilePath)] -> m (GL.Program ty)
build s = send (Build s pure)

use :: Has Program sig m => GL.Program ty -> ProgramT ty m a -> m a
use p (ProgramT m) = send (Use p (runReader p m) pure)

set :: forall name a ty m sig . (GL.HasUniform name a ty, HasProgram ty m, Has Program sig m) => a -> m ()
set v = askProgram >>= \ p -> send (Set p (GL.Var @name v) (pure ()))


class HasProgram (ty :: Context) (m :: * -> *) | m -> ty where
  askProgram :: m (GL.Program ty)


newtype ProgramT (ty :: Context) m a = ProgramT { runProgramT :: ReaderC (GL.Program ty) m a }
  deriving (Applicative, Functor, Monad, MonadIO, MonadTrans)

instance Algebra sig m => Algebra sig (ProgramT ty m) where
  alg = ProgramT . send . handleCoercible

instance Algebra sig m => HasProgram ty (ProgramT ty m) where
  askProgram = ProgramT ask
