{-# LANGUAGE DataKinds, DeriveFunctor, ExistentialQuantification, FlexibleContexts, FlexibleInstances, FunctionalDependencies, GeneralizedNewtypeDeriving, KindSignatures, LambdaCase, ScopedTypeVariables, StandaloneDeriving, TypeApplications, TypeOperators, UndecidableInstances #-}
module GL.Effect.Program
( -- * Program effect
  Program(..)
, build
, use
, set
, HasProgram(..)
, ProgramT(..)
  -- * Re-exports
, Algebra
, Has
, run
) where

import Control.Algebra
import Control.Carrier.Reader
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import qualified GL.Program as GL
import qualified GL.Shader.DSL as DSL

data Program m k
  = forall u i o . Build (DSL.Shader u i o) (GL.Program u -> m k)
  | forall ty a . Use (GL.Program ty) (m a) (a -> m k)
  | forall ty . DSL.Vars ty => Set (GL.Program ty) (ty Maybe) (m k)

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


build :: forall u i o m sig . Has Program sig m => DSL.Shader u i o -> m (GL.Program u)
build s = send (Build s pure)

use :: Has Program sig m => GL.Program ty -> ProgramT ty m a -> m a
use p (ProgramT m) = send (Use p (runReader p m) pure)

set :: (DSL.Vars ty, HasProgram ty m, Has Program sig m) => ty Maybe -> m ()
set v = askProgram >>= \ p -> send (Set p v (pure ()))


class HasProgram (ty :: (* -> *) -> *) (m :: * -> *) | m -> ty where
  askProgram :: m (GL.Program ty)


newtype ProgramT (ty :: (* -> *) -> *) m a = ProgramT { runProgramT :: ReaderC (GL.Program ty) m a }
  deriving (Applicative, Functor, Monad, MonadIO, MonadTrans)

instance Algebra sig m => Algebra sig (ProgramT ty m) where
  alg = ProgramT . send . handleCoercible

instance Algebra sig m => HasProgram ty (ProgramT ty m) where
  askProgram = ProgramT ask
