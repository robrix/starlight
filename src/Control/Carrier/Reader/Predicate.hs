{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Control.Carrier.Reader.Predicate
( -- * Predicates
  runPredicate
, Predicate(..)
) where

import Control.Algebra
import Control.Applicative (Alternative)
import Control.Carrier.Reader

runPredicate :: i -> Predicate i a -> Maybe a
runPredicate i (Predicate m) = runReader i m

newtype Predicate i a = Predicate (ReaderC i Maybe a)
  deriving (Alternative, Applicative, Functor, Monad)

instance Algebra (Reader i) (Predicate i) where
  alg = Predicate . send . handleCoercible
