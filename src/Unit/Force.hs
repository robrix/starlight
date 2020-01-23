{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
module Unit.Force
( Newtons
, module Unit
, module Unit.Multiple
) where

import Unit
import Unit.Algebra
import Unit.Length
import Unit.Mass
import Unit.Multiple
import Unit.Time

type Newtons = Kilo Grams :*: Metres :/: Seconds :^: 2
