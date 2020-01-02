module Control.Carrier.Reader.Predicate
( -- * Predicates
  Predicate(..)
) where

newtype Predicate i a = Predicate (i -> Maybe a)
