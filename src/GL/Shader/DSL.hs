{-# LANGUAGE DataKinds, ExplicitForAll, KindSignatures, TypeApplications, TypeOperators #-}
module GL.Shader.DSL
( Shader
, Expr
, Ref
, uniform
, input
, output
, main
) where

import Data.DSL
import GL.Shader (Type(..))
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

data Ref t

uniform :: forall n t k u i o . (Expr k t -> Shader k u i o) -> Shader k ((n '::: t) ': u) i o
uniform _ = undefined

input :: forall n t k u i o . (Expr k t -> Shader k u i o) -> Shader k u ((n '::: t) ': i) o
input _ = undefined

output :: forall n t k u i o . ((Expr k t -> Expr k ()) -> Shader k u i o) -> Shader k u i ((n '::: t) ': o)
output _ = undefined

main :: Expr k () -> Shader k u i o
main _ = undefined


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
