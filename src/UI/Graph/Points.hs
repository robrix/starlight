{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
module UI.Graph.Points
( shader
, U(..)
, I(..)
, O(..)
) where

import GHC.Generics (Generic)
import GL.Object
import GL.Shader.DSL

shader :: Shader U I O
shader = V vertex $ F fragment Nil where
  vertex U { matrix, pointSize } I { pos } None = do
    gl_Position .= vec4 (vec3 ((matrix !* vec3 pos 1) ^. _xy) 0) 1
    gl_PointSize .= pointSize

  fragment U { colour } None O { fragColour } = do
    p <- let' "p" (gl_PointCoord - vec2 0.5 0.5)
    iff (norm p `gt` 1)
      discard
      (do
        mag <- let' "mag" (norm p * 2)
        fragColour .= vec4 (colour ^. _xyz) (1 - mag * mag * mag / 2))


data U v = U
  { matrix    :: v (M33 Float)
  , pointSize :: v Float
  , colour    :: v (Colour Float)
  }
  deriving (Generic)

instance Vars U

newtype I v = I { pos :: v (V2 Float) }
  deriving (Generic)

instance Vars I

deriving instance Bind (v (V2 Float)) => Bind (I v)

newtype O v = O { fragColour :: v (Colour Float) }
  deriving (Generic)

instance Vars O
