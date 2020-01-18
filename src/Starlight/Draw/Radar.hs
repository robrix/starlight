{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
module Starlight.Draw.Radar
( draw
, Starlight.Draw.Radar.run
, Drawable
, V(..)
) where

import           Control.Carrier.Reader
import           Control.Effect.Finally
import           Control.Effect.Lift
import           Control.Effect.Profile
import           Control.Effect.State (put)
import           Control.Effect.Trace
import           Control.Lens (Lens', lens, (%~), (&), (.~), (?~), (^.))
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
import           GL.Shader.Vars (makeVars)
import           Linear.Exts as Linear hiding ((!*))
import           Starlight.Actor
import qualified Starlight.Body as B
import           Starlight.Character
import qualified Starlight.Ship as S
import           Starlight.System
import           Starlight.View
import           UI.Colour
import qualified UI.Window as Window
import           Unit.Angle
import           Unit.Length

draw
  :: ( Has Check sig m
     , Has (Lift IO) sig m
     , Has Profile sig m
     , Has (Reader Drawable) sig m
     , Has (Reader (System B.StateVectors)) sig m
     , Has (Reader View) sig m
     )
  => m ()
draw = ask >>= \ Drawable{ radarProgram, targetProgram, array, buffer } -> bindArray array . B.bindBuffer buffer $ do
  system@System{ bodies } <- ask @(System B.StateVectors)
  view@View{ scale, focus = here } <- ask
  let npcs     = system^.npcs_
      vertices = verticesForShips scale npcs <> verticesForBodies bodies
      vars = makeVars (const Nothing)
        & matrix_ ?~ tmap realToFrac (transformToWindow view)
        & here_   ?~ here
        & scale_  ?~ realToFrac (lengthToWindowPixels view)

  measure "realloc/copy" $ do
    B.realloc @'B.Array (length vertices) B.Static B.Draw
    B.copy @'B.Array 0 vertices

  use radarProgram $ do
    put vars
    -- FIXME: skip blips for extremely distant objects
    -- FIXME: blips should shadow more distant blips
    -- FIXME: fade colour with distance
    -- FIXME: IFF
    measure "bodies & npcs" $
      drawArrays Points (Interval 0 (I (length vertices)))

  use targetProgram $ do
    put vars

    measure "targets" $
      for_ (system^.player_.target_ >>= (`elemIndex` drop 1 (identifiers system))) $ \ index ->
        drawArrays Points (Interval (I index) (I index + 1))

run :: (Effect sig, Has Check sig m, Has Finally sig m, Has (Lift IO) sig m, Has Trace sig m) => ReaderC Drawable m a -> m a
run m = do
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


data Blip a = Blip { scale :: Double, value :: a }
  deriving (Generic)

instance HasActor a => HasActor (Blip a) where
  actor_ = lens get set where
    get Blip{ scale, value } = value^.actor_ & magnitude_ %~ (^* scale)
    set Blip{ scale, value } actor = Blip{ scale, value = value & actor_ .~ (actor & magnitude_ %~ (^* (1/scale))) }

instance HasColour a => HasColour (Blip a) where
  colour_ = field @"value".colour_

verticesForBodies :: Foldable t => t B.StateVectors -> [V I]
verticesForBodies vs =
  [ V{ there = I (there^._xy), r = I (b^.magnitude_), colour = I colour }
  | b@B.StateVectors{ body = B.Body{ colour }, actor = Actor{ position = there } } <- toList vs
  ]

-- FIXME: take ship profile into account
verticesForShips :: Foldable t => Double -> t Character -> [V I]
verticesForShips scale cs =
  [ V{ there = I (there^._xy), r = I (c^.magnitude_ ^/ scale), colour = I colour }
  | c@Character{ actor = Actor{ position = there }, ship = S.Ship { colour } } <- toList cs
  ]


vertex' :: U (Expr 'Vertex) -> Stage V IG
vertex' u = vertex (\ V{ there, r, colour } IG{ colour2, sweep } -> main $ do
  there <- let' "there" (D.coerce (there - here u))
  d     <- let' "d"     (D.norm there)
  dir   <- let' "dir"   (there D.^* (1/d))
  let perp v = dvec2 [negate (v D.^.D._y), v D.^.D._x]
      angleOf vec = D.coerce $ atan2' (vec D.^.D._y) (vec D.^.D._x)
      wrap mn mx x = ((x + mx) `mod'` (mx - mn)) + mn
  edge  <- let' "edge"  (perp dir D.^* D.coerce r D.^* 0.5 + there)
  angle <- let' "angle" (angleOf (vec2 [there]))
  radius <- let' "radius" (D.min' ((\ U{ scale } -> scale) u * float d) radius)
  minSweep <- let' "minSweep" (minBlipSize / (2 * pi * D.coerce radius))
  sweep .= (minSweep `D.max'` abs (wrap (-pi) pi (angleOf (vec2 [edge]) - angle)))
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
          gl_Position .= ext4 (vec3 [0]) 1
          colour3 .= Var "colour2[0]"
        emitVertex $ do
          gl_Position .= D.coerce (matrix u) D.!*! mat4 [rot theta] !* Var "gl_in[0].gl_Position"
          colour3 .= Var "colour2[0]"
        i += 1)

  >>> fragment' where
  count = 1


data U v = U
  { matrix :: v (Transform Float ClipUnits Window.Pixels)
  , here   :: v (V2 (Mega Metres Double))
  , scale  :: v Float
  }
  deriving (Generic)

instance Vars U

matrix_ :: Lens' (U v) (v (Transform Float ClipUnits Window.Pixels))
matrix_ = field @"matrix"

here_ :: Lens' (U v) (v (V2 (Mega Metres Double)))
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
  { there  :: v (V2 (Mega Metres Double)) -- location of object
  , r      :: v (Mega Metres Double)      -- radius of object
  , colour :: v (Colour Float)
  }
  deriving (Generic)

instance Vars V
deriving via Fields V instance Storable (V I)
