{-# LANGUAGE RankNTypes #-}
module Control.Carrier.State.ST
( -- * State carrier
  StateC(..)
  -- * State effect
, module Control.Effect.State
) where

import Control.Carrier.Reader
import Control.Effect.State
import Control.Monad.ST
import Data.STRef

newtype StateC s a = StateC (forall t . ReaderC (STRef t s) (ST t) a)
