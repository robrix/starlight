{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
module UI.Path
( Component(..)
, Path
, pathTriangles
) where

import Geometry.Triangle
import Linear.V2

data Component v n
  = M (v n)
  | L (v n)
  | Q (v n) (v n)
  deriving (Eq, Functor, Show)

type Path v a = [Component v a]


pathTriangles :: Num a => Path V2 a -> [(Triangle a, Kind)]
pathTriangles = go False 0 0 where
  go notFirst first current = \case
    M v:rest ->                                                                             go False v     v  rest
    L v:rest
      | notFirst  -> (Triangle first current v,  Solid)                                   : go True  first v  rest
      | otherwise ->                                                                        go True  first v  rest
    Q v1 v2:rest
      | notFirst  -> (Triangle first current v2, Solid) : (Triangle current v1 v2, Curve) : go True  first v2 rest
      | otherwise ->                                      (Triangle current v1 v2, Curve) : go True  first v2 rest
    [] ->                                                                                   []
