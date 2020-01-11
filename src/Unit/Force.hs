{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
module Unit.Force
( Newton
, module Unit
) where

import Unit.Length
import Unit.Mass
import Unit.Time
import Unit

type Newton = Kilo Grams :*: Metres :/: (Seconds :*: Seconds)
