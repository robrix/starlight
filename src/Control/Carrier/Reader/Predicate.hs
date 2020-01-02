{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Control.Carrier.Reader.Predicate
( -- * Predicates
  runPredicate
, Predicate(..)
) where

import Control.Algebra
import Control.Applicative (Alternative(..), liftA2)
import Control.Effect.Reader
import Control.Monad (ap, liftM)

runPredicate :: i -> Predicate i a -> Maybe a
runPredicate i (Predicate m) = m i

newtype Predicate i a = Predicate (i -> Maybe a)

instance Functor (Predicate i) where
  fmap = liftM

instance Applicative (Predicate i) where
  pure a = Predicate (\ _ -> Just a)
  (<*>) = ap

instance Alternative (Predicate i) where
  empty = Predicate (\ _ -> Nothing)
  Predicate l <|> Predicate r = Predicate (liftA2 (<|>) l r)

instance Monad (Predicate i) where
  Predicate m >>= f = Predicate (\ i -> m i >>= runPredicate i . f)

instance Algebra (Reader i) (Predicate i) where
  alg = \case
    Ask       k -> Predicate Just                     >>= k
    Local f m k -> Predicate ((`runPredicate` m) . f) >>= k
