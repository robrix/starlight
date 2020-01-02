module Control.Effect.Lens.Exts
( (&~)
, module Control.Effect.Lens
) where

import Control.Carrier.State.ST.Strict
import Control.Effect.Lens

(&~) :: s -> StateC s a -> s
(&~) = execState

infixl 1 &~
