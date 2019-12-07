module Control.Carrier.State.IORef
( -- * State carrier
  StateC(..)
) where

import Control.Carrier.Reader
import Data.IORef

newtype StateC s m a = StateC (ReaderC (IORef s) m a)
