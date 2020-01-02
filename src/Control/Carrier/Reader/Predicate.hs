module Control.Carrier.Reader.Predicate
( -- * Predicates
  Predicate(..)
) where

import Control.Applicative (Alternative(..), liftA2)
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
