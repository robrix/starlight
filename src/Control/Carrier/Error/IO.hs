module Control.Carrier.Error.IO
( -- * Error carrier
  ErrorC(..)
  -- * Error effect
, module Control.Effect.Error
) where

import Control.Effect.Error

newtype ErrorC e m a = ErrorC (m a)
