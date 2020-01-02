{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
module GL
( Capability(..)
, GLC(..)
) where

import Control.Monad.IO.Class
import GL.Enum as GL
import Graphics.GL.Core41

data Capability
  = Blend                      -- ^ GL_BLEND
  | ColourLogicOp              -- ^ GL_COLOR_LOGIC_OP
  | CullFace                   -- ^ GL_CULL_FACE
  | DepthClamp                 -- ^ GL_DEPTH_CLAMP
  | DepthTest                  -- ^ GL_DEPTH_TEST
  | Dither                     -- ^ GL_DITHER
  | FramebufferSRGB            -- ^ GL_FRAMEBUFFER_SRGB
  | LineSmooth                 -- ^ GL_LINE_SMOOTH
  | Multisample                -- ^ GL_MULTISAMPLE
  | PolygonOffsetFill          -- ^ GL_POLYGON_OFFSET_FILL
  | PolygonOffsetLine          -- ^ GL_POLYGON_OFFSET_LINE
  | PolygonOffsetPoint         -- ^ GL_POLYGON_OFFSET_POINT
  | PolygonSmooth              -- ^ GL_POLYGON_SMOOTH
  | PrimitiveRestart           -- ^ GL_PRIMITIVE_RESTART
  | RasterizerDiscard          -- ^ GL_RASTERIZER_DISCARD
  | SampleAlphaToCoverage      -- ^ GL_SAMPLE_ALPHA_TO_COVERAGE
  | SampleAlphaToOne           -- ^ GL_SAMPLE_ALPHA_TO_ONE
  | SampleCoverage             -- ^ GL_SAMPLE_COVERAGE
  | SampleShading              -- ^ GL_SAMPLE_SHADING
  | SampleMask                 -- ^ GL_SAMPLE_MASK
  | ScissorTest                -- ^ GL_SCISSOR_TEST
  | StencilTest                -- ^ GL_STENCIL_TEST
  | TextureCubeMapSeamless     -- ^ GL_TEXTURE_CUBE_MAP_SEAMLESS
  | ProgramPointSize           -- ^ GL_PROGRAM_POINT_SIZE
  deriving (Show)

instance GL.Enum Capability where
  glEnum = \case
    Blend                      -> GL_BLEND
    ColourLogicOp              -> GL_COLOR_LOGIC_OP
    CullFace                   -> GL_CULL_FACE
    DepthClamp                 -> GL_DEPTH_CLAMP
    DepthTest                  -> GL_DEPTH_TEST
    Dither                     -> GL_DITHER
    FramebufferSRGB            -> GL_FRAMEBUFFER_SRGB
    LineSmooth                 -> GL_LINE_SMOOTH
    Multisample                -> GL_MULTISAMPLE
    PolygonOffsetFill          -> GL_POLYGON_OFFSET_FILL
    PolygonOffsetLine          -> GL_POLYGON_OFFSET_LINE
    PolygonOffsetPoint         -> GL_POLYGON_OFFSET_POINT
    PolygonSmooth              -> GL_POLYGON_SMOOTH
    PrimitiveRestart           -> GL_PRIMITIVE_RESTART
    RasterizerDiscard          -> GL_RASTERIZER_DISCARD
    SampleAlphaToCoverage      -> GL_SAMPLE_ALPHA_TO_COVERAGE
    SampleAlphaToOne           -> GL_SAMPLE_ALPHA_TO_ONE
    SampleCoverage             -> GL_SAMPLE_COVERAGE
    SampleShading              -> GL_SAMPLE_SHADING
    SampleMask                 -> GL_SAMPLE_MASK
    ScissorTest                -> GL_SCISSOR_TEST
    StencilTest                -> GL_STENCIL_TEST
    TextureCubeMapSeamless     -> GL_TEXTURE_CUBE_MAP_SEAMLESS
    ProgramPointSize           -> GL_PROGRAM_POINT_SIZE


newtype GLC m a = GLC (m a)
  deriving (Applicative, Functor, Monad, MonadIO)
