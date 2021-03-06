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
import           GL.Shader.DSL hiding (norm, (!*), (!*!))
import qualified GL.Shader.DSL as D
import           Prelude hiding (break)
import qualified Starlight.Body as Body
import           Starlight.View
import qualified UI.Drawable as UI

run
  :: ( Has Check sig m
     , Has Finally sig m
     , Has (Lift IO) sig m
     , Has Trace sig m
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
    <<< Body.transform v
    <<< Body.toBodySpace v)
  colour_ ?= colour

  drawArraysInstanced LineLoop range 3


newtype Drawable = Drawable { getDrawable :: UI.Drawable U V Frag }


vertices :: [V I]
vertices = coerce @[V2 (I Double)] $ circle 1 128

range :: Interval I Int
range = 0...length vertices


shader :: D.Shader shader => shader U V Frag
shader
  =   vertex (\ U{ matrix } V{ pos } D.None -> main $ do
    let cos90 = 6.123233995736766e-17
    m <- var "m" matrix
    switch gl_InstanceID
      [ (Just 1, do
        m *= D.mkRotation (m4
          1 0     0     0
          0 cos90 (-1)  0
          0 1     cos90 0
          0 0     0     1)
        break)
      , (Just 2, do
        m *= D.mkRotation (m4
          cos90 0 1     0
          0     1 0     0
          (-1)  0 cos90 0
          0     0 0     1)
        break)
      ]
    gl_Position .= cast @_ @(V4 (ClipUnits Float)) (get m D.>* dext4 (dext3 pos 0) 1))

  >>> fragment (\ U{ colour } D.None Frag{ fragColour } -> main $
    fragColour .= colour)


data U v = U
  { matrix :: v (Transform V4 Double Body.BodyUnits ClipUnits)
  , colour :: v (Colour Float)
  }
  deriving (Generic)

instance D.Vars U

matrix_ :: Lens' (U v) (v (Transform V4 Double Body.BodyUnits ClipUnits))
matrix_ = field @"matrix"

colour_ :: Lens' (U v) (v (Colour Float))
colour_ = field @"colour"


newtype V v = V { pos :: v (V2 (Body.BodyUnits Double)) }
  deriving (Generic)

instance D.Vars V

deriving via Fields V instance Storable (V I)
