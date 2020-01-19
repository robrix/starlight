{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
module Unit.Force
( Force
, Newtons
, module Unit
, module Unit.Multiple
) where

import Unit.Length
import Unit.Mass
import Unit.Time
import Unit
import Unit.Algebra
import Unit.Multiple

data Force a

type Newtons = Kilo Grams :*: Metres :/: Seconds :^: 2
