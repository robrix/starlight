module Control.Carrier.Reader.Predicate
( -- * Predicates
  Predicate(..)
) where

import Control.Monad (ap, liftM)

runPredicate :: i -> Predicate i a -> Maybe a
runPredicate i (Predicate m) = m i

newtype Predicate i a = Predicate (i -> Maybe a)

instance Functor (Predicate i) where
  fmap = liftM

instance Applicative (Predicate i) where
  pure a = Predicate (\ _ -> Just a)
  (<*>) = ap

instance Monad (Predicate i) where
  Predicate m >>= f = Predicate (\ i -> m i >>= runPredicate i . f)
