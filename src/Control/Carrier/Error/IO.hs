{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
module Control.Carrier.Error.IO
( -- * Error carrier
  runError
, ErrorC(..)
  -- * Error effect
, module Control.Effect.Error
) where

import Control.Algebra
import Control.Effect.Error
import Control.Effect.Lift
import Control.Exception.Lift
import Control.Monad.IO.Class
import Control.Monad.Trans.Class

runError :: (Exception e, Has (Lift IO) sig m) => ErrorC e m a -> m (Either e a)
runError (ErrorC m) = try m

newtype ErrorC e m a = ErrorC (m a)
  deriving (Applicative, Functor, Monad, MonadFail, MonadIO)

instance MonadTrans (ErrorC e) where
  lift = ErrorC

instance (Exception e, Has (Lift IO) sig m) => Algebra (Error e :+: sig) (ErrorC e m) where
  alg = \case
    L (L (Throw e))     -> throwIO e
    L (R (Catch m h k)) -> (m `catch` h) >>= k
    R other             -> ErrorC (send (handleCoercible other))
