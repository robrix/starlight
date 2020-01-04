{-# LANGUAGE RankNTypes #-}
module Control.Effect.Lens.Exts
( (&~)
, zoom
, zoomEach
, (~>)
, (<~>)
, (<->)
, module Control.Effect.Lens
) where

import Control.Carrier.State.ST.Strict as ST
import Control.Carrier.State.Strict as Strict
import Control.Effect.Lens
import Control.Lens (Getting, Lens', set, (^.))

(&~) :: s -> ST.StateC s a -> s
(&~) = ST.execState

infixl 1 &~


zoom :: Has (State s) sig m => Lens' s a -> Strict.StateC a m () -> m ()
zoom lens action = lens <~> (`Strict.execState` action)

infixr 2 `zoom`


zoomEach :: (Has (State s) sig m, Traversable t) => Lens' s (t a) -> Strict.StateC a m () -> m ()
zoomEach lens action = lens <~> traverse (`Strict.execState` action)

infixr 2 `zoomEach`


(~>) :: Has (State s) sig m => Getting a s a -> (a -> m b) -> m b
lens ~> act = use lens >>= act

infixr 2 ~>

(<~>) :: Has (State s) sig m => Lens' s a -> (a -> m a) -> m ()
lens <~> act = lens <~ lens ~> act

infixr 2 <~>


(<->) :: Functor m => Lens' s a -> (a -> m a) -> (s -> m s)
(lens <-> act) s = ($ s) . set lens <$> act (s^.lens)

infixr 2 <->
