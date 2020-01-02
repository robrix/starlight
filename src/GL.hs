{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE UndecidableInstances #-}
module GL
( Capability(..)
, Capabilities(..)
, enabled_
, runGLC
, GLC(..)
) where

import Control.Algebra
import Control.Monad.IO.Class
import Data.Maybe (fromMaybe)
import GHC.Generics (Generic)
import GL.Enum as GL
import GL.Shader.DSL (Vars)
import Graphics.GL.Core41
import Lens.Micro (Lens', lens)

data Capability
  = Blend                      -- ^ GL_BLEND
  -- ColourLogicOp              -- ^ GL_COLOR_LOGIC_OP
  -- CullFace                   -- ^ GL_CULL_FACE
  | DepthClamp                 -- ^ GL_DEPTH_CLAMP
  -- DepthTest                  -- ^ GL_DEPTH_TEST
  -- Dither                     -- ^ GL_DITHER
  -- FramebufferSRGB            -- ^ GL_FRAMEBUFFER_SRGB
  -- LineSmooth                 -- ^ GL_LINE_SMOOTH
  -- Multisample                -- ^ GL_MULTISAMPLE
  -- PolygonOffsetFill          -- ^ GL_POLYGON_OFFSET_FILL
  -- PolygonOffsetLine          -- ^ GL_POLYGON_OFFSET_LINE
  -- PolygonOffsetPoint         -- ^ GL_POLYGON_OFFSET_POINT
  -- PolygonSmooth              -- ^ GL_POLYGON_SMOOTH
  -- PrimitiveRestart           -- ^ GL_PRIMITIVE_RESTART
  -- RasterizerDiscard          -- ^ GL_RASTERIZER_DISCARD
  -- SampleAlphaToCoverage      -- ^ GL_SAMPLE_ALPHA_TO_COVERAGE
  -- SampleAlphaToOne           -- ^ GL_SAMPLE_ALPHA_TO_ONE
  -- SampleCoverage             -- ^ GL_SAMPLE_COVERAGE
  -- SampleShading              -- ^ GL_SAMPLE_SHADING
  -- SampleMask                 -- ^ GL_SAMPLE_MASK
  | ScissorTest                -- ^ GL_SCISSOR_TEST
  -- StencilTest                -- ^ GL_STENCIL_TEST
  -- TextureCubeMapSeamless     -- ^ GL_TEXTURE_CUBE_MAP_SEAMLESS
  | ProgramPointSize           -- ^ GL_PROGRAM_POINT_SIZE
  deriving (Show)

instance GL.Enum Capability where
  glEnum = \case
    Blend                      -> GL_BLEND
    -- ColourLogicOp              -> GL_COLOR_LOGIC_OP
    -- CullFace                   -> GL_CULL_FACE
    DepthClamp                 -> GL_DEPTH_CLAMP
    -- DepthTest                  -> GL_DEPTH_TEST
    -- Dither                     -> GL_DITHER
    -- FramebufferSRGB            -> GL_FRAMEBUFFER_SRGB
    -- LineSmooth                 -> GL_LINE_SMOOTH
    -- Multisample                -> GL_MULTISAMPLE
    -- PolygonOffsetFill          -> GL_POLYGON_OFFSET_FILL
    -- PolygonOffsetLine          -> GL_POLYGON_OFFSET_LINE
    -- PolygonOffsetPoint         -> GL_POLYGON_OFFSET_POINT
    -- PolygonSmooth              -> GL_POLYGON_SMOOTH
    -- PrimitiveRestart           -> GL_PRIMITIVE_RESTART
    -- RasterizerDiscard          -> GL_RASTERIZER_DISCARD
    -- SampleAlphaToCoverage      -> GL_SAMPLE_ALPHA_TO_COVERAGE
    -- SampleAlphaToOne           -> GL_SAMPLE_ALPHA_TO_ONE
    -- SampleCoverage             -> GL_SAMPLE_COVERAGE
    -- SampleShading              -> GL_SAMPLE_SHADING
    -- SampleMask                 -> GL_SAMPLE_MASK
    ScissorTest                -> GL_SCISSOR_TEST
    -- StencilTest                -> GL_STENCIL_TEST
    -- TextureCubeMapSeamless     -> GL_TEXTURE_CUBE_MAP_SEAMLESS
    ProgramPointSize           -> GL_PROGRAM_POINT_SIZE


data Capabilities v = Capabilities
  { blend            :: v Bool
  , depthClamp       :: v Bool
  , programPointSize :: v Bool
  , scissorTest      :: v Bool
  }
  deriving (Generic)

instance Vars Capabilities

enabled_ :: Capability -> Lens' (Capabilities Maybe) Bool
enabled_ = \case
  Blend            -> lens (orFalse . blend)            (\ u v -> u { blend            = Just v })
  DepthClamp       -> lens (orFalse . depthClamp)       (\ u v -> u { depthClamp       = Just v })
  ProgramPointSize -> lens (orFalse . programPointSize) (\ u v -> u { programPointSize = Just v })
  ScissorTest      -> lens (orFalse . scissorTest)      (\ u v -> u { scissorTest      = Just v })
  where
  orFalse = fromMaybe False


runGLC :: GLC m a -> m a
runGLC (GLC m) = m

newtype GLC m a = GLC (m a)
  deriving (Applicative, Functor, Monad, MonadIO)

instance Algebra sig m => Algebra sig (GLC m) where
  alg = GLC . send . handleCoercible
