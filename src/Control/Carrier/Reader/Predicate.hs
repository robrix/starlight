module Control.Carrier.Reader.Predicate
( -- * Predicates
  Predicate(..)
) where

runPredicate :: i -> Predicate i a -> Maybe a
runPredicate i (Predicate m) = m i

newtype Predicate i a = Predicate (i -> Maybe a)
