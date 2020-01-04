{-# LANGUAGE RankNTypes #-}
module Control.Effect.Lens.Exts
( (&~)
, zoom
, zoomEach
, (~>)
, (<~>)
, (<--)
, (-->)
, (<->)
, module Control.Effect.Lens
) where

import Control.Carrier.State.ST.Strict as ST
import Control.Carrier.State.Strict as Strict
import Control.Effect.Lens
import Control.Lens (ASetter, Getting, Lens', set, (^.))

(&~) :: s -> ST.StateC s a -> s
(&~) = ST.execState

infixl 1 &~


zoom :: Has (State s) sig m => Lens' s a -> Strict.StateC a m () -> m ()
zoom lens action = lens <~> (`Strict.execState` action)

infixr 2 `zoom`


zoomEach :: (Has (State s) sig m, Traversable t) => Lens' s (t a) -> Strict.StateC a m () -> m ()
zoomEach lens action = lens <~> traverse (`Strict.execState` action)

infixr 2 `zoomEach`


-- | Compose a getter onto the input of a Kleisli arrow and run it on the 'State'.
(~>) :: Has (State s) sig m => Getting a s a -> (a -> m b) -> m b
lens ~> act = use lens >>= act

infixr 2 ~>

-- | Compose a lens onto either side of a Kleisli arrow and run it on the 'State'.
(<~>) :: Has (State s) sig m => Lens' s a -> (a -> m a) -> m ()
lens <~> act = lens <~ lens ~> act

infixr 2 <~>


-- | Compose a setter onto the output of a Kleisli arrow.
--
-- By analogy with '<~':
--
-- > lens '<~' act = 'get' >>= lens '<--' 'const' act '>>=' 'put'
(<--) :: Functor m => ASetter s s a b -> (s -> m b) -> (s -> m s)
(lens <-- act) s = ($ s) . set lens <$> act s

infixr 2 <--

-- | Compose a getter onto the input of a Kleisli arrow.
--
-- By analogy with '~>':
--
-- > lens '~>' act = 'get' '>>=' lens '-->' act
(-->) :: Getting a s a -> (a -> m b) -> (s -> m b)
(lens --> act) s = act (s^.lens)

infixr 2 -->

-- | Compose a lens onto either side of a Kleisli arrow.
--
-- By analogy with '<~>':
--
-- > lens '<~>' act = 'get' '>>=' lens '<->' act '>>=' 'put'
(<->) :: Functor m => Lens' s a -> (a -> m a) -> (s -> m s)
lens <-> act = lens <-- lens --> act

infixr 2 <->
