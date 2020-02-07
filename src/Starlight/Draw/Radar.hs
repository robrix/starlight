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
{-# LANGUAGE TypeOperators #-}
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
import           Control.Lens (Lens', (&), (?~), (^.))
import           Data.Foldable (for_, toList)
import           Data.Functor.I
import           Data.Functor.Interval
import           Data.Generics.Product.Fields
import           Data.List (findIndex, sortOn)
import qualified Data.Map as Map
import           Data.Ord (Down(..))
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
import           Starlight.Identifier (CharacterIdentifier(..), Identifier(..))
import           Starlight.Physics
import           Starlight.System
import           Starlight.View
import           UI.Colour
import qualified UI.Window as Window
import           Unit.Algebra
import           Unit.Angle

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
  view@View{ shipScale, focus = here } <- ask
  let npcs     = system^.npcs_
      -- FIXME: this is a lot of poorly motivated faffing about to get the units lined up just so we can multiply the magnitude by the scale
      blips    = sortOn (Down . qd here . (^.position_._xy)) (map (uncurry (blipFor shipScale . C . NPC)) (Map.toList npcs)) <> map (uncurry (blipFor 1 . B)) (Map.toList bodies)
      vertices = verticesForBlips blips
      vars     = makeVars (const Nothing)
        & matrix_ ?~ tmap realToFrac (transformToWindow view)
        & here_   ?~ here
        & scale_  ?~ realToFrac (lengthToWindowPixels view)

  measure "realloc/copy" $ do
    B.realloc @'B.Array (length vertices) B.Static B.Draw
    B.copy @'B.Array 0 vertices

  use radarProgram $ do
    put vars
    -- FIXME: skip blips for extremely distant objects
    -- FIXME: fade colour with distance
    -- FIXME: IFF
    measure "bodies & npcs" $
      drawArrays Points (Interval 0 (I (length vertices)))

  use targetProgram $ do
    put vars

    measure "targets" $
      for_ (system^.player_.target_ >>= (`findIndex` blips) . (. identifier) . (==)) $ \ index ->
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


data Blip = Blip
  { scale      :: I Double
  , identifier :: Identifier
  , actor      :: Actor
  , colour     :: Colour Float
  }
  deriving (Generic)

instance HasActor Blip where
  actor_ = field @"actor"

instance HasColour Blip where
  colour_ = field @"colour"

blipFor :: (HasActor t, HasColour t) => I Double -> Identifier -> t -> Blip
blipFor scale identifier t = Blip{ scale, identifier, actor = t^.actor_, colour = t^.colour_ }

-- FIXME: take ship profile into account
verticesForBlips :: Foldable t => t Blip -> [V I]
verticesForBlips bs =
  [ V{ there = I (b^.position_._xy), r = I (b^.magnitude_ .*. scale * 0.5), colour = I (b^.colour_) }
  | b@Blip{ scale } <- toList bs
  ]


vertex' :: U (Expr 'Vertex) -> Stage V IG
vertex' U{ here, scale } = vertex (\ V{ there, r, colour } IG{ colour2, sweep } -> main $ do
  there <- let' "there" (there - here)
  d     <- let' "d"     (D.norm there)
  let angleOf vec = atan2' (vec D.^.D._y) (vec D.^.D._x)
  angle <- let' "angle" (angleOf (vec2 [there]))
  radius <- let' "radius" (D.min' (scale * float d) radius)
  minSweep <- let' "minSweep" (minBlipSize / (2 * pi * D.coerce radius))
  iff (r `gt` d)
    (sweep .= pi/2)
    (sweep .= (minSweep `D.max'` abs (D.coerce (asin (float (r/d))))))
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
  , here   :: v (V2 (Distance Double))
  , scale  :: v Float
  }
  deriving (Generic)

instance Vars U

matrix_ :: Lens' (U v) (v (Transform Float ClipUnits Window.Pixels))
matrix_ = field @"matrix"

here_ :: Lens' (U v) (v (V2 (Distance Double)))
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
  { there  :: v (V2 (Distance Double)) -- location of object
  , r      :: v (Distance Double)      -- radius of object
  , colour :: v (Colour Float)
  }
  deriving (Generic)

instance Vars V
deriving via Fields V instance Storable (V I)
