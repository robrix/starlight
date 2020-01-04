{-# LANGUAGE RankNTypes #-}
module Control.Effect.Lens.Exts
( (&~)
, zoom
, zoomEach
, (<~)
, module Control.Effect.Lens
) where

import Control.Carrier.State.ST.Strict as ST
import Control.Carrier.State.Strict as Strict
import Control.Effect.Lens
import Control.Lens (ASetter, Lens')

(&~) :: s -> ST.StateC s a -> s
(&~) = ST.execState

infixl 1 &~


zoom :: Has (State s) sig m => Lens' s a -> Strict.StateC a m () -> m ()
zoom lens action = lens <~ (use lens >>= (`Strict.execState` action))

infixr 2 `zoom`


zoomEach :: (Has (State s) sig m, Traversable t) => Lens' s (t a) -> Strict.StateC a m () -> m ()
zoomEach lens action = lens <~ (use lens >>= traverse (`Strict.execState` action))

infixr 2 `zoomEach`


(<~) :: (Has (State s) sig m) => ASetter s s a b -> m b -> m ()
lens <~ act = act >>= assign lens

infixr 2 <~
