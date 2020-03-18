{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
module Control.Carrier.Error.IO
( -- * Error carrier
  runError
, ErrorC(ErrorC)
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

newtype ErrorC e m a = ErrorC { runErrorC :: m a }
  deriving (Applicative, Functor, Monad, MonadFail, MonadIO)

instance MonadTrans (ErrorC e) where
  lift = ErrorC

instance (Exception e, Has (Lift IO) sig m) => Algebra (Error e :+: sig) (ErrorC e m) where
  alg hdl sig ctx = case sig of
    L (L (Throw e))   -> throwIO e
    L (R (Catch m h)) -> hdl (m <$ ctx) `catch` (hdl . (<$ ctx) . h)
    R other           -> ErrorC (alg (runErrorC . hdl) other ctx)
