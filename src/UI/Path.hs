{-# LANGUAGE DeriveFunctor, TypeApplications #-}
module UI.Path
( Path(..)
, pathTriangles
) where

import Geometry.Triangle
import Linear.V2

data Path v n
  = M (v n) (Path v n)
  | L (v n) (Path v n)
  | Q (v n) (v n) (Path v n)
  | Z
  deriving (Eq, Functor, Show)


pathTriangles :: Num a => Path V2 a -> [(Triangle a, Kind)]
pathTriangles = go False 0 0 where
  go notFirst first current p = case p of
    M v rest ->                                                                             go False v     v  rest
    L v rest
      | notFirst  -> (Triangle first current v,  Solid)                                   : go True  first v  rest
      | otherwise ->                                                                        go True  first v  rest
    Q v1 v2 rest
      | notFirst  -> (Triangle first current v2, Solid) : (Triangle current v1 v2, Curve) : go True  first v2 rest
      | otherwise ->                                      (Triangle current v1 v2, Curve) : go True  first v2 rest
    Z ->                                                                                    []
