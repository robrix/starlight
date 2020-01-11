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
, drawBody
) where

import           Control.Carrier.Reader
import           Control.Effect.Finally
import           Control.Effect.Lens ((?=))
import           Control.Effect.Lift
import           Control.Effect.Profile
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
import           GL.Shader.DSL hiding (coerce, (!*), (!*!))
import qualified GL.Shader.DSL as D
import           Linear.Exts
import           Prelude hiding (break)
import           Starlight.Actor
import qualified Starlight.Body as Body
import           Starlight.System
import           Starlight.View
import qualified UI.Drawable as UI
import           Unit.Length

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


drawBody
  :: ( Has Check sig m
     , Has (Lift IO) sig m
     , Has Profile sig m
     , Has (Reader Drawable) sig m
     , Has (Reader (System Body.StateVectors)) sig m
     , Has (Reader View) sig m
     )
  => Body.StateVectors
  -> m ()
drawBody Body.StateVectors{ body = Body.Body{ radius, colour }, transform, actor = Actor{ rotation } } = measure "body" . UI.using getDrawable $ do
  vs@View{ focus } <- ask
  systemTrans <- asks (systemTrans @Body.StateVectors)
  matrix_
    ?=  scaleToViewZoomed vs
    !*! systemTrans
    !*! translated3 (ext (negated (prj <$> unP focus)) 0) -- transform to the origin
    !*! transform
    !*! scaled (ext (pure @V3 (prj radius)) 1)
    !*! mkTransformation rotation 0
  colour_ ?= colour

  drawArraysInstanced LineLoop range 3


newtype Drawable = Drawable { getDrawable :: UI.Drawable U V O }


vertices :: [V Identity]
vertices = coerce @[V4 Float] . map (`ext` V2 0 1) $ circle 1 128

range :: Interval Identity Int
range = Interval 0 (Identity (length vertices))


shader :: D.Shader U V O
shader = program $ \ u
  ->  vertex (\ V{ pos } D.None -> main $ do
    let cos90 = 6.123233995736766e-17
    m <- var "m" (matrix u)
    switch gl_InstanceID
      [ (Just 1, m *= mat4 (vec4 1 0 0 0) (vec4 0 cos90 (-1) 0) (vec4 0 1 cos90 0) (vec4 0 0 0 1) >> break)
      , (Just 2, m *= mat4 (vec4 cos90 0 1 0) (vec4 0 1 0 0) (vec4 (-1) 0 cos90 0) (vec4 0 0 0 1) >> break)
      ]
    gl_Position .= get m D.!* pos)

  >>> fragment (\ D.None O { fragColour } -> main $
    fragColour .= colour u)


data U v = U
  { matrix :: v (M44 Float)
  , colour :: v (Colour Float)
  }
  deriving (Generic)

instance D.Vars U

matrix_ :: Lens' (U v) (v (M44 Float))
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
