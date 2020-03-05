{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Control.Carrier.Reader.Relation
( -- * Relations
  runRelation
, Relation(Relation)
, expect
) where

import Control.Algebra
import Control.Applicative (Alternative)
import Control.Carrier.Reader
import Control.Effect.Lens (view)
import Control.Effect.Sum (inj)
import Control.Lens (Getting)
import Control.Monad (guard, (<=<))

runRelation :: i -> Relation i a -> Maybe a
runRelation i (Relation m) = runReader i m

newtype Relation i a = Relation { runRelationC :: ReaderC i Maybe a }
  deriving (Alternative, Applicative, Functor, Monad)

instance Algebra (Reader i) (Relation i) where
  alg ctx hdl = Relation . alg ctx (runRelationC . hdl) . inj


expect :: (Alternative m, Has (Reader r) sig m) => Getting Bool r Bool -> m ()
expect = guard <=< view
