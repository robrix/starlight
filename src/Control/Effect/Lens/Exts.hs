{-# LANGUAGE RankNTypes #-}
module Control.Effect.Lens.Exts
( (~>)
, (<~>)
, (<--)
, (-->)
, (<->)
, asserting
, module Control.Effect.Lens
) where

import Control.Effect.State
import Control.Effect.Lens
import Control.Exception (assert)
import Control.Lens (ASetter, Getting, Iso', Lens', iso, set, (^.))
import GHC.Stack (HasCallStack, withFrozenCallStack)

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


asserting :: HasCallStack => (a -> Bool) -> Iso' a a
asserting pred = withFrozenCallStack $ iso id (assert . pred <*> id)
