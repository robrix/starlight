{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Control.Carrier.Error.IO
( -- * Error carrier
  runError
, ErrorC(..)
  -- * Error effect
, module Control.Effect.Error
) where

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
