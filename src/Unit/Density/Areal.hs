{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
module Unit.Density.Areal
( Density
) where

import Unit.Algebra
import Unit.Count
import Unit.Length

type Density sym = Count sym :/: Metres :^: 2
