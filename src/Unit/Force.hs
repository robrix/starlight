{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
module Unit.Force
( Newtons
, module Unit
, module Unit.Multiple
) where

import Unit.Length
import Unit.Mass
import Unit.Time
import Unit
import Unit.Algebra
import Unit.Multiple

type Newtons = Kilo Grams :*: Metres :*: Inv Seconds :*: Inv Seconds
