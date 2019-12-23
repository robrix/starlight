{-# LANGUAGE DataKinds, ExplicitForAll, FlexibleInstances, FunctionalDependencies, KindSignatures, TypeApplications, TypeOperators #-}
module GL.Shader.DSL
( Shader
, Expr
, Ref
, Prj
, uniform
, Uniforms(..)
, input
, output
, main
, let'
, gl_Position
, gl_PointSize
, (.=)
, (|*)
) where

import Data.DSL
import GL.Shader (Type(..))
import Linear.Exts
import Linear.Matrix
import Linear.V2
import Linear.V3
import Linear.V4
import UI.Colour

data Shader (k :: Type) (u :: Context) (i :: Context) (o :: Context)
data Expr (k :: Type) a

instance Num (Expr k a) where
  _ + _ = undefined
  _ * _ = undefined
  _ - _ = undefined
  signum _ = undefined
  negate _ = undefined
  abs _ = undefined
  fromInteger _ = undefined

instance Fractional (Expr k a) where
  _ / _ = undefined
  recip _ = undefined
  fromRational _ = undefined

instance Floating (Expr k a) where
  pi = undefined
  exp _ = undefined
  log _ = undefined
  sqrt _ = undefined
  _ ** _ = undefined
  logBase _ _ = undefined
  sin _ = undefined
  cos _ = undefined
  tan _ = undefined
  asin _ = undefined
  acos _ = undefined
  atan _ = undefined
  sinh _ = undefined
  cosh _ = undefined
  tanh _ = undefined
  asinh _ = undefined
  acosh _ = undefined
  atanh _ = undefined

instance Ext (Expr k (V2 a)) (Expr k a) (Expr k (V3 a)) where
  ext _ = undefined

instance Ext (Expr k (V2 a)) (Expr k (V2 a)) (Expr k (V4 a)) where
  ext _ = undefined

instance Ext (Expr k (V3 a)) (Expr k a) (Expr k (V4 a)) where
  ext _ = undefined

data Ref t

data Prj s t

uniform :: forall n t k u i o . (Expr k t -> Shader k u i o) -> Shader k ((n '::: t) ': u) i o
uniform _ = undefined

class Uniforms k u i o a where
  uniforms :: a -> Shader k u i o

instance Uniforms k u i o (Shader k u i o) where
  uniforms = id

instance Uniforms k u i o a => Uniforms k ((n '::: t) ': u) i o (Expr k t -> a) where
  uniforms with = uniform (uniforms . with)

input :: forall n t k u i o . (Expr k t -> Shader k u i o) -> Shader k u ((n '::: t) ': i) o
input _ = undefined

output :: forall n t k u i o . ((Expr k t -> Expr k ()) -> Shader k u i o) -> Shader k u i ((n '::: t) ': o)
output _ = undefined

main :: Expr k () -> Shader k u i o
main _ = undefined


let' :: Expr k a -> (Expr k a -> Expr k b) -> Expr k b
let' _ _ = undefined


gl_Position :: Expr 'Vertex (Ref (V4 Float))
gl_Position = undefined

gl_PointSize :: Expr 'Vertex (Ref Float)
gl_PointSize = undefined


(.=) :: Expr k (Ref a) -> Expr k a -> Expr k ()
_ .= _ = undefined

infixr 4 .=


(|*) :: Expr k (M33 Float) -> Expr k (V3 Float) -> Expr k (V3 Float)
_ |* _ = undefined

infixl 7 |*


_shipVertex
  :: Shader
    'Vertex
    '[ "matrix" '::: M33 Float ]
    '[ "position2" '::: V2 Float ]
    '[]
_shipVertex
  = uniform
  $ \ matrix ->
    input
  $ \ pos ->
    main $ gl_Position .= ext (matrix |* ext pos 1) 1

_shipFragment
  :: Shader
    'Fragment
    '[ "colour"     '::: Colour Float ]
    '[]
    '[ "fragColour" '::: Colour Float ]
_shipFragment
  = uniform
  $ \ colour ->
    output
  $ \ fragColour ->
    main $ fragColour colour
