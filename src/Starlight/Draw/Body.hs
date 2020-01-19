{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
module Starlight.Draw.Body
( Starlight.Draw.Body.run
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
import           Data.Functor.I
import           Data.Functor.Interval hiding (range)
import           Data.Generics.Product.Fields
import           Foreign.Storable
import           Geometry.Circle
import           GHC.Generics (Generic)
import           GL.Array
import           GL.Effect.Check
import           GL.Shader.DSL hiding (coerce, norm, (!*), (!*!))
import qualified GL.Shader.DSL as D
import           Linear.Exts
import           Prelude hiding (break)
import qualified Starlight.Body as Body
import           Starlight.View
import qualified UI.Drawable as UI

run
  :: ( Has Check sig m
     , Has Finally sig m
     , Has (Lift IO) sig m
     , Has Trace sig m
     , Effect sig
     )
  => ReaderC Drawable m a
  -> m a
run = UI.loadingDrawable Drawable shader vertices


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


newtype Drawable = Drawable { getDrawable :: UI.Drawable U V Frag }


vertices :: [V I]
vertices = coerce @[V4 (I Double)] . map (`ext` V2 0 (I 1 :: I Double)) $ circle (I 1 :: I Double) 128

range :: Interval I Int
range = Interval 0 (I (length vertices))


shader :: D.Shader U V Frag
shader = program $ \ u
  ->  vertex (\ V{ pos } D.None -> main $ do
    let cos90 = 6.123233995736766e-17
    m <- var "m" (D.coerce (matrix u))
    switch gl_InstanceID
      [ (Just 1, m *= dmat4 [dvec4 [1, 0, 0, 0], dvec4 [0, cos90, -1, 0], dvec4 [0, 1, cos90, 0], dvec4 [0, 0, 0, 1]] >> break)
      , (Just 2, m *= dmat4 [dvec4 [cos90, 0, 1, 0], dvec4 [0, 1, 0, 0], dvec4 [-1, 0, cos90, 0], dvec4 [0, 0, 0, 1]] >> break)
      ]
    gl_Position .= vec4 [get m D.!* pos])

  >>> fragment (\ D.None Frag{ fragColour } -> main $
    fragColour .= colour u)


data U v = U
  { matrix :: v (Transform Double ClipUnits Body.BodyUnits)
  , colour :: v (Colour Float)
  }
  deriving (Generic)

instance D.Vars U

matrix_ :: Lens' (U v) (v (Transform Double ClipUnits Body.BodyUnits))
matrix_ = field @"matrix"

colour_ :: Lens' (U v) (v (Colour Float))
colour_ = field @"colour"


newtype V v = V { pos :: v (V4 Double) }
  deriving (Generic)

instance D.Vars V

deriving via Fields V instance Storable (V I)
