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

runError :: (Exception e, Has (Lift IO) sig m) => ErrorC e m a -> m (Either e a)
runError (ErrorC m) = try m

newtype ErrorC e m a = ErrorC (m a)
