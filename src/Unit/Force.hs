{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
module Unit.Force
( Newtons
, module Unit
) where

import Unit.Length
import Unit.Mass
import Unit.Time
import Unit

type Newtons = Kilo Grams :*: Metres :/: (Seconds :*: Seconds)
