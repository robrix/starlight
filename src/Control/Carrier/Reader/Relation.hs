{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Control.Carrier.Reader.Relation
( -- * Relations
  runRelation
, Relation(..)
, expect
) where

import Control.Algebra
import Control.Applicative (Alternative)
import Control.Carrier.Reader
import Control.Effect.Lens (view)
import Control.Lens (Getting)
import Control.Monad (guard, (<=<))

runRelation :: i -> Relation i a -> Maybe a
runRelation i (Relation m) = runReader i m

newtype Relation i a = Relation (ReaderC i Maybe a)
  deriving (Alternative, Applicative, Functor, Monad)

instance Algebra (Reader i) (Relation i) where
  alg = Relation . send . handleCoercible


expect :: (Alternative m, Has (Reader r) sig m) => Getting Bool r Bool -> m ()
expect = guard <=< view
