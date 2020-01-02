{-# LANGUAGE RankNTypes #-}
module Control.Effect.Lens.Exts
( (&~)
, zoom
, zoomEach
, module Control.Effect.Lens
) where

import Control.Carrier.State.ST.Strict as ST
import Control.Carrier.State.Strict as Strict
import Control.Effect.Lens
import Lens.Micro (Lens')

(&~) :: s -> ST.StateC s a -> s
(&~) = ST.execState

infixl 1 &~


zoom :: Has (State s) sig m => Lens' s a -> Strict.StateC a m () -> m ()
zoom lens action = use lens >>= (`Strict.execState` action) >>= assign lens

infixr 2 `zoom`


zoomEach :: (Has (State s) sig m, Traversable t) => Lens' s (t a) -> Strict.StateC a m () -> m ()
zoomEach lens action = use lens >>= traverse (`Strict.execState` action) >>= assign lens
