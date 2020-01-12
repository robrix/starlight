module Control.Carrier.State.STM.TVar
( StateC(..)
) where

import Control.Carrier.Reader
import Control.Concurrent.STM.TVar

newtype StateC s m a = StateC (ReaderC (TVar s) m a)
