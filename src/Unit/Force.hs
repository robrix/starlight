{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
module Unit.Force
( Force
, Newtons
, module Unit
, module Unit.Multiple
) where

import Unit
import Unit.Algebra
import Unit.Length
import Unit.Mass
import Unit.Multiple
import Unit.Time

type Force mass length time = mass :*: length :/: time :^: 2

type Newtons = Kilo Grams :*: Metres :/: Seconds :^: 2
