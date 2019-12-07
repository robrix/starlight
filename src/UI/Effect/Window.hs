{-# LANGUAGE DeriveFunctor, ExistentialQuantification, StandaloneDeriving #-}
module UI.Effect.Window
( -- * Window effect
  Window(..)
  -- * Re-exports
, Algebra
, Has
, run
) where

import Control.Algebra

data Window m k
  = forall a . Draw (m a) (a -> m k)

deriving instance Functor m => Functor (Window m)
