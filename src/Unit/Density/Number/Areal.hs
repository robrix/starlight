{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
module Unit.Density.Number.Areal
( Density
, module Unit
, module Unit.Algebra
, module Unit.Multiple
) where

import Unit
import Unit.Algebra
import Unit.Count
import Unit.Length
import Unit.Multiple

type Density sym = Count sym :/: Metres :^: 2
