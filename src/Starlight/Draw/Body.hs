{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}
module Starlight.Draw.Body
( runBody
, Drawable
, draw
) where

import           Control.Carrier.Reader
import           Control.Effect.Finally
import           Control.Effect.Lens ((?=))
import           Control.Effect.Lift
import           Control.Effect.Trace
import           Control.Lens (Lens')
import           Data.Coerce
import           Data.Functor.Identity
import           Data.Functor.Interval
import           Data.Generics.Product.Fields
import           Foreign.Storable
import           Geometry.Circle
import           GHC.Generics (Generic)
import           GL.Array
import           GL.Effect.Check
import           GL.Object
import           GL.Shader.DSL hiding (coerce, norm, (!*), (!*!))
import qualified GL.Shader.DSL as D
import           Linear.Exts
import           Prelude hiding (break)
import qualified Starlight.Body as Body
import           Starlight.View
import qualified UI.Drawable as UI

runBody
  :: ( Has Check sig m
     , Has Finally sig m
     , Has (Lift IO) sig m
     , Has Trace sig m
     , Effect sig
     )
  => ReaderC Drawable m a
  -> m a
runBody = UI.loadingDrawable Drawable shader vertices


draw
  :: ( Has Check sig m
     , Has (Lift IO) sig m
     , Has (Reader Drawable) sig m
     , Has (Reader View) sig m
     )
  => Body.StateVectors
  -> m ()
draw v@Body.StateVectors{ body = Body.Body{ colour } } = UI.using getDrawable $ do
  view <- ask
  matrix_ ?=
    (   transformToSystem view
    >>> Body.transform v
    >>> Body.toBodySpace v)
  colour_ ?= colour

  drawArraysInstanced LineLoop range 3


newtype Drawable = Drawable { getDrawable :: UI.Drawable U V O }


vertices :: [V Identity]
vertices = coerce @[V4 Float] . map (`ext` V2 0 (1 :: Float)) $ circle 1 128

range :: Interval Identity Int
range = Interval 0 (Identity (length vertices))


shader :: D.Shader U V O
shader = program $ \ u
  ->  vertex (\ V{ pos } D.None -> main $ do
    let cos90 = 6.123233995736766e-17
    m <- var "m" (D.coerce (matrix u))
    switch gl_InstanceID
      [ (Just 1, m *= mat4 (vec4 1 0 0 0) (vec4 0 cos90 (-1) 0) (vec4 0 1 cos90 0) (vec4 0 0 0 1) >> break)
      , (Just 2, m *= mat4 (vec4 cos90 0 1 0) (vec4 0 1 0 0) (vec4 (-1) 0 cos90 0) (vec4 0 0 0 1) >> break)
      ]
    gl_Position .= get m D.!* pos)

  >>> fragment (\ D.None O { fragColour } -> main $
    fragColour .= colour u)


data U v = U
  { matrix :: v (Transform ClipUnits Body.BodyUnits)
  , colour :: v (Colour Float)
  }
  deriving (Generic)

instance D.Vars U

matrix_ :: Lens' (U v) (v (Transform ClipUnits Body.BodyUnits))
matrix_ = field @"matrix"

colour_ :: Lens' (U v) (v (Colour Float))
colour_ = field @"colour"


newtype V v = V { pos :: v (V4 Float) }
  deriving (Generic)

instance D.Vars V

deriving instance Bind     (v (V4 Float)) => Bind     (V v)
deriving instance Storable (v (V4 Float)) => Storable (V v)

newtype O v = O { fragColour :: v (Colour Float) }
  deriving (Generic)

instance D.Vars O
