{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
module Unit.Force
( Newton
) where

import Unit.Length
import Unit.Mass
import Unit.Time

type Newton = Kilo Grams :*: Metres :/: Seconds :^: 2
