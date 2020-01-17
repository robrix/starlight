{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
module Starlight.Draw.Radar
( drawRadar
, runRadar
, Drawable
, V(..)
) where

import           Control.Carrier.Reader
import           Control.Effect.Finally
import           Control.Effect.Lens ((?=))
import           Control.Effect.Lift
import           Control.Effect.Profile
import           Control.Effect.Trace
import           Control.Lens (Lens', (^.))
import           Data.Foldable (for_, toList)
import           Data.Functor.I
import           Data.Functor.Interval
import           Data.Generics.Product.Fields
import           Data.List (elemIndex)
import           Foreign.Storable (Storable(..))
import           GHC.Generics (Generic)
import           GL.Array
import           GL.Buffer as B
import           GL.Effect.Check
import           GL.Program
import           GL.Shader (Type(..))
import           GL.Shader.DSL hiding (coerce, norm, (!*!), (^*), (^.), _a, _xy, _xyz)
import qualified GL.Shader.DSL as D
import           Linear.Exts as Linear hiding ((!*))
import           Starlight.Actor
import qualified Starlight.Body as B
import           Starlight.Character
import qualified Starlight.Ship as S
import           Starlight.System
import           Starlight.View
import qualified UI.Window as Window
import           Unit.Angle
import           Unit.Length

drawRadar
  :: ( Has Check sig m
     , Has (Lift IO) sig m
     , Has Profile sig m
     , Has (Reader Drawable) sig m
     , Has (Reader (System B.StateVectors)) sig m
     , Has (Reader View) sig m
     )
  => m ()
drawRadar = ask >>= \ Drawable{ radarProgram, targetProgram, array, buffer } -> bindArray array . B.bindBuffer buffer $ do
  system@System{ bodies } <- ask @(System B.StateVectors)
  view@View{ scale, focus = here } <- ask
  let npcs     = system^.npcs_
      vertices = verticesForShips scale npcs <> verticesForBodies bodies

  measure "realloc/copy" $ do
    B.realloc (length vertices) B.Static B.Draw
    B.copy 0 vertices

  use radarProgram $ do
    matrix_ ?= tmap realToFrac (transformToWindow view)
    here_   ?= (fmap realToFrac <$> here)
    scale_  ?= realToFrac (lengthToWindowPixels view)

    -- FIXME: skip blips for extremely distant objects
    -- FIXME: blips should shadow more distant blips
    -- FIXME: fade colour with distance
    -- FIXME: IFF
    measure "bodies & npcs" $
      drawArrays Points (Interval 0 (I (length vertices)))

  use targetProgram $ do
    matrix_ ?= tmap realToFrac (transformToWindow view)
    here_   ?= (fmap realToFrac <$> here)
    scale_  ?= realToFrac (lengthToWindowPixels view)

    measure "targets" $
      for_ (system^.player_.target_ >>= (`elemIndex` drop 1 (identifiers system))) $ \ index ->
        drawArrays Points (Interval (I index) (I index + 1))

runRadar :: (Effect sig, Has Check sig m, Has Finally sig m, Has (Lift IO) sig m, Has Trace sig m) => ReaderC Drawable m a -> m a
runRadar m = do
  radarProgram  <- build radarShader
  targetProgram <- build targetShader
  (buffer, array) <- load []
  runReader Drawable{ radarProgram, targetProgram, array, buffer } m


data Drawable = Drawable
  { radarProgram  :: Program U V Frag
  , targetProgram :: Program U V Frag
  , array         :: Array (V I)
  , buffer        :: B.Buffer 'B.Array (V I)
  }


verticesForBodies :: Foldable t => t B.StateVectors -> [V I]
verticesForBodies vs =
  [ V{ there = I (realToFrac <$> (there^._xy)), r = I (realToFrac <$> (b^.actor_.magnitude_)), colour = I colour }
  | b@B.StateVectors{ body = B.Body{ colour }, actor = Actor{ position = there } } <- toList vs
  ]

-- FIXME: take ship profile into account
verticesForShips :: Foldable t => Double -> t Character -> [V I]
verticesForShips scale cs =
  [ V{ there = I (realToFrac <$> (there^._xy)), r = I (realToFrac <$> (c^.actor_.magnitude_ ^/ scale)), colour = I colour }
  | c@Character{ actor = Actor{ position = there }, ship = S.Ship { colour } } <- toList cs
  ]


vertex' :: U (Expr 'Vertex) -> Stage V IG
vertex' u = vertex (\ V{ there, r, colour } IG{ colour2, sweep } -> main $ do
  there <- let' "there" (D.coerce there - D.coerce (here u))
  d     <- let' "d"     (D.norm there)
  dir   <- let' "dir"   (there D.^* (1/d))
  let perp v = vec2 [negate (v D.^.D._y), v D.^.D._x]
      angleOf vec = D.coerce $ atan2' (vec D.^.D._y) (vec D.^.D._x)
      wrap mn mx x = ((x + mx) `mod'` (mx - mn)) + mn
  edge  <- let' "edge"  (perp dir D.^* D.coerce r D.^* 0.5 + there)
  angle <- let' "angle" (angleOf there)
  radius <- let' "radius" (D.min' (Starlight.Draw.Radar.scale u * d) radius)
  minSweep <- let' "minSweep" (minBlipSize / (2 * pi * D.coerce radius))
  sweep .= (minSweep `D.max'` abs (wrap (-pi) pi (angleOf edge - angle)))
  pos   <- let' "pos"   (vec2 [cos angle, sin angle] D.^* D.coerce radius)
  colour2 .= colour
  gl_Position .= ext4 (ext3 pos 0) 1) where
  minBlipSize = 16
  radius = 300

fragment' :: Stage IF Frag
fragment' = fragment (\ IF{ colour3 } Frag{ fragColour } -> main $ fragColour .= colour3)

radarShader :: Shader U V Frag
radarShader = program $ \ u
  ->  vertex' u

  >>> geometry (\ IG{} IF{ colour3 } -> do
    primitiveIn Points
    primitiveOut LineStrip (count * 2 + 1)
    main $ do
      let rot theta = mat2
            [ vec2 [ cos theta, -sin theta ]
            , vec2 [ sin theta,  cos theta ]
            ]
      emitPrimitive $ do
        i <- var @Int "i" (-fromIntegral count)
        while (get i `lt` (fromIntegral count + 1)) $ do
          emitVertex $ do
            theta <- let' "theta" (float (get i) / float (fromIntegral count) * Var "sweep[0]")
            gl_Position .= D.coerce (matrix u) D.!*! mat4 [rot theta] !* Var "gl_in[0].gl_Position"
            colour3 .= Var "colour2[0]"
          i += 1)

  >>> fragment' where
  count = 16


targetShader :: Shader U V Frag
targetShader = program $ \ u
  ->  vertex' u

  >>> geometry (\ IG{} IF{ colour3 } -> do
    primitiveIn Points
    primitiveOut LineStrip ((count * 2 + 1) * 2)
    main $ do
      let rot theta = mat2
            [ vec2 [ cos theta, -sin theta ]
            , vec2 [ sin theta,  cos theta ]
            ]
      i <- var @Int "i" (-fromIntegral count)
      while (get i `lt` (fromIntegral count + 1)) . emitPrimitive $ do
        theta <- let' "theta" (float (get i) / float (fromIntegral count) * Var "sweep[0]")
        emitVertex $ do
          gl_Position .= D.coerce (matrix u) D.!*! mat4[mat3[0.0]] D.!*! mat4 [rot theta] !* Var "gl_in[0].gl_Position"
          colour3 .= Var "colour2[0]"
        emitVertex $ do
          gl_Position .= D.coerce (matrix u) D.!*! mat4 [rot theta] !* Var "gl_in[0].gl_Position"
          colour3 .= Var "colour2[0]"
        i += 1)

  >>> fragment' where
  count = 1


data U v = U
  { matrix :: v (Transform Float ClipUnits Window.Pixels)
  , here   :: v (V2 (Mega Metres Float))
  , scale  :: v Float
  }
  deriving (Generic)

instance Vars U

matrix_ :: Lens' (U v) (v (Transform Float ClipUnits Window.Pixels))
matrix_ = field @"matrix"

here_ :: Lens' (U v) (v (V2 (Mega Metres Float)))
here_ = field @"here"

scale_ :: Lens' (U v) (v Float)
scale_ = field @"scale"


data IG v = IG
  { colour2 :: v (Colour Float)
  , sweep   :: v (Radians Float)
  }
  deriving (Generic)

instance Vars IG

newtype IF v = IF { colour3 :: v (Colour Float) }
  deriving (Generic)

instance Vars IF


data V v = V
  { there  :: v (V2 (Mega Metres Float)) -- location of object
  , r      :: v (Mega Metres Float)      -- radius of object
  , colour :: v (Colour Float)
  }
  deriving (Generic)

instance Vars V
deriving via Fields V instance Storable (V I)
