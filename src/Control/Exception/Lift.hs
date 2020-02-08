{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
module Control.Exception.Lift
( E.Exception(..)
, E.SomeException(..)
, throwIO
, catch
, catches
, Handler(..)
, handle
, try
, mask
, bracket
, bracket_
, finally
, onException
) where

import           Control.Effect.Lift
import qualified Control.Exception as E

-- | See @"Control.Exception".'E.throwIO'@.
throwIO :: (E.Exception e, Has (Lift IO) sig m) => e -> m a
throwIO = sendM . E.throwIO

-- | See @"Control.Exception".'E.catch'@.
catch :: (E.Exception e, Has (Lift IO) sig m) => m a -> (e -> m a) -> m a
catch m h = liftWith $ \ ctx run -> run (m <$ ctx) `E.catch` (run . (<$ ctx) . h)

-- | See @"Control.Exception".'E.catches'@.
catches :: Has (Lift IO) sig m => m a -> [Handler m a] -> m a
catches m hs = liftWith $ \ ctx run ->
  E.catches (run (m <$ ctx)) (map (\ (Handler h) -> E.Handler (run . (<$ ctx) . h)) hs)

-- | See @"Control.Exception".'E.Handler'@.
data Handler m a
  = forall e . E.Exception e => Handler (e -> m a)

deriving instance Functor m => Functor (Handler m)

-- | See @"Control.Exception".'E.handle'@.
handle :: (E.Exception e, Has (Lift IO) sig m) => (e -> m a) -> m a -> m a
handle h m = liftWith $ \ ctx run -> (run . (<$ ctx) . h) `E.handle` run (m <$ ctx)

-- | See @"Control.Exception".'E.try'@.
try :: (E.Exception e, Has (Lift IO) sig m) => m a -> m (Either e a)
try = handle (pure . Left) . fmap Right

-- | See @"Control.Exception".'E.mask'@.
mask :: Has (Lift IO) sig m => ((forall a . m a -> m a) -> m b) -> m b
mask with = liftWith $ \ ctx run -> E.mask $ \ restore ->
  run (with (\ m -> liftWith $ \ ctx' run' -> restore (run' (m <$ ctx'))) <$ ctx)

-- | See @"Control.Exception".'E.bracket'@.
bracket
  :: Has (Lift IO) sig m
  => m a
  -> (a -> m b)
  -> (a -> m c)
  -> m c
bracket acquire release m = mask $ \ restore -> do
  a <- acquire
  r <- restore (m a) `onException` release a
  r <$ release a

-- | See @"Control.Exception".'E.bracket_'@.
bracket_
  :: Has (Lift IO) sig m
  => m a
  -> m b
  -> m c
  -> m c
bracket_ before after thing = bracket before (const after) (const thing)

-- | See @"Control.Exception".'E.finally'@.
finally
  :: Has (Lift IO) sig m
  => m a
  -> m b
  -> m a
finally m sequel = mask $ \ restore -> (restore m `onException` sequel) <* sequel

-- | See @"Control.Exception".'E.onException'@.
onException :: Has (Lift IO) sig m => m a -> m b -> m a
onException io what = io `catch` \e -> what >> throwIO (e :: E.SomeException)
