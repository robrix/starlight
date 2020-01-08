{-# LANGUAGE RankNTypes #-}
module Control.Carrier.Empty.CPS
( -- * Empty carrier
  EmptyC(EmptyC)
  -- * Empty effect
, module Control.Effect.Empty
) where

import Control.Effect.Empty

newtype EmptyC m a = EmptyC (forall r . (a -> m (Maybe r)) -> m (Maybe r))
