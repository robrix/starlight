{-# LANGUAGE DeriveFunctor, GADTs, FlexibleInstances, RecordWildCards, ScopedTypeVariables #-}
module Main where

import Control.Applicative
import qualified Control.Concurrent as CC
import qualified Control.Exception as E
import Control.Monad
import Data.Bits
import Data.Char
import Data.Foldable
import Data.Functor.Identity
import Data.Int
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.Semigroup
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Vector ((!?))
import Data.Word
import qualified Foreign.C.String as C
import Foreign.Ptr
import GHC.Stack
import GL.Array
import GL.Error
import GL.Framebuffer
import GL.Object
import GL.Program
import GL.Scalar
import GL.Texture
import GL.TextureUnit
import GL.Uniform
import Graphics.GL.Core41
import Graphics.GL.Types
import Linear.Matrix as Linear
import Linear.V2 as Linear
import Linear.V3 as Linear
import Linear.V4 as Linear
import Linear.Vector as Linear
import qualified Opentype.Fileformat as O
import SDL.Event
import SDL.Init
import qualified SDL.Raw as SDL
import System.Exit

main :: HasCallStack => IO ()
main = do
  Just tahoma <- readTypeface "/Library/Fonts/Tahoma.ttf"
  let glyphs = Main.glyphs tahoma "hs"
  [textVertex, textFragment, glyphVertex, glyphFragment] <- traverse readFile ["text-vertex.glsl", "text-fragment.glsl", "glyph-vertex.glsl", "glyph-fragment.glsl"]
  CC.runInBoundThread $ C.withCString "Text" $ \ name -> do
    _ <- SDL.init SDL.SDL_INIT_EVERYTHING >>= checkWhen (< 0)

    SDL.SDL_GL_CONTEXT_MAJOR_VERSION `set` 4
    SDL.SDL_GL_CONTEXT_MINOR_VERSION `set` 1
    SDL.SDL_GL_CONTEXT_PROFILE_MASK `set` SDL.SDL_GL_CONTEXT_PROFILE_CORE

    SDL.SDL_GL_RED_SIZE   `set` 8
    SDL.SDL_GL_GREEN_SIZE `set` 8
    SDL.SDL_GL_BLUE_SIZE  `set` 8
    SDL.SDL_GL_ALPHA_SIZE `set` 8
    SDL.SDL_GL_DEPTH_SIZE `set` 16

    SDL.SDL_GL_DOUBLEBUFFER `set` fromEnum True

    ignoreEventsOfTypes
      [ SDL.SDL_FINGERMOTION
      , SDL.SDL_FINGERUP
      , SDL.SDL_FINGERDOWN ]

    withWindow name (fromIntegral <$> windowSize) flags (\ window ->
      withContext window (\ _ ->
        let rect    = Var "rect"    :: Var (V4 Float)
            colour  = Var "colour"  :: Var (V4 Float)
            sampler = Var "sampler" :: Var TextureUnit
            matrix3 = Var "matrix3" :: Var (M33 Float)
            instances = foldl (combineInstances (V2 72 72)) [] glyphs
            instanceBounds' = fromMaybe (error "wtf") (sfoldMap instanceBounds instances)
            geometry = Geometry GL_TRIANGLES . instanceGeometry <$> instances
            vertices = foldl combineGeometry (ArrayVertices [] 0 []) (Geometry GL_TRIANGLE_STRIP
              [ V4 (-1) (-1) 0 1
              , V4   1  (-1) 0 1
              , V4 (-1)   1  0 1
              , V4   1    1  0 1 :: V4 Float
              ] : geometry) in
        withArray (arrayVertices vertices) $ \ array ->
        withBuiltProgram [(GL_VERTEX_SHADER, textVertex), (GL_FRAGMENT_SHADER, textFragment)] $ \ textProgram ->
        withBuiltProgram [(GL_VERTEX_SHADER, glyphVertex), (GL_FRAGMENT_SHADER, glyphFragment)] $ \ glyphProgram ->
        with $ \ texture ->
        with $ \ framebuffer ->
        forever $ do
          glViewport 0 0 (2 * width) (2 * height)

          glDisable GL_BLEND
          glBlendFunc GL_ONE GL_ZERO

          setClearColour (V4 1 1 1 1)
          glClear GL_COLOR_BUFFER_BIT

          let V2 sx sy = V2 2 (-2) / fmap fromIntegral windowSize
              transformA = V3 (V3  1  0 (-1))
                              (V3  0  1   1)
                              (V3  0  0   1) !*! scaled (V3 sx sy 1)
          -- print transformA
          checkingGLError $ glBindFramebuffer GL_FRAMEBUFFER (unFramebuffer framebuffer)
          checkingGLError $ glBindTexture GL_TEXTURE_2D (unTexture texture)
          checkingGLError $ glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MAG_FILTER GL_NEAREST
          checkingGLError $ glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MIN_FILTER GL_NEAREST
          checkingGLError $ glTexParameteri GL_TEXTURE_2D GL_TEXTURE_WRAP_S GL_CLAMP_TO_EDGE
          checkingGLError $ glTexParameteri GL_TEXTURE_2D GL_TEXTURE_WRAP_T GL_CLAMP_TO_EDGE
          checkingGLError $ glTexImage2D GL_TEXTURE_2D 0 GL_RGBA width height 0 GL_RGBA GL_UNSIGNED_BYTE nullPtr

          checkingGLError $ glFramebufferTexture2D GL_FRAMEBUFFER GL_COLOR_ATTACHMENT0 GL_TEXTURE_2D (unTexture texture) 0

          glViewport 0 0 (2 * width) (2 * height)

          setClearColour (V4 0 0 0 0)
          glClear GL_COLOR_BUFFER_BIT

          glEnable GL_BLEND
          glBlendFunc GL_ONE GL_ONE

          checkingGLError $ glBindVertexArray (unArray array)

          glUseProgram (unProgram glyphProgram)
          let s = 1 / 2
          for_ (zip instances (tail (arrayRanges vertices))) $ \ (Instance{..}, range) ->
            for_ (zip [0..] jitterPattern) $ \ (j, offset) -> do
              when (j `mod` 2 == (0 :: Int)) $
                setUniformValue glyphProgram colour (V4 (if j == 0 then 1 else 0) (if j == 2 then 1 else 0) (if j == 4 then 1 else 0) 1.0)
              setUniformValue glyphProgram matrix3 $ (transformA `translate` instanceOffset `translate` (offset * s)) !*! scaled (V3 (instanceScale ^. _x) (instanceScale ^. _y) 1)
              drawRange range

          glBindFramebuffer GL_FRAMEBUFFER 0
          glBlendFunc GL_ZERO GL_SRC_COLOR

          glUseProgram (unProgram textProgram)
          let rect' = V4
                (    fromIntegral (floor   (minX instanceBounds') :: Int) / fromIntegral width)
                (1 - fromIntegral (ceiling (maxY instanceBounds') :: Int) / fromIntegral height)
                (    fromIntegral (ceiling (maxX instanceBounds') :: Int) / fromIntegral width)
                (1 - fromIntegral (floor   (minY instanceBounds') :: Int) / fromIntegral height)
          print instanceBounds'
          setUniformValue textProgram rect rect'
          setUniformValue textProgram colour (V4 0 0 0 1)
          glActiveTexture GL_TEXTURE0
          glBindTexture GL_TEXTURE_2D (unTexture texture)
          setUniformValue textProgram sampler (TextureUnit 0)

          -- drawRange (head (arrayRanges vertices))
          traverse_ drawRange (tail (arrayRanges vertices))

          event <- waitEvent
          case eventPayload event of
            QuitEvent -> do
              quit
              exitSuccess
            _ -> pure ()

          SDL.glSwapWindow window))
    `E.finally`
      SDL.quit
  where flags = foldr (.|.) 0
          [ SDL.SDL_WINDOW_OPENGL
          , SDL.SDL_WINDOW_SHOWN
          , SDL.SDL_WINDOW_RESIZABLE
          , SDL.SDL_WINDOW_ALLOW_HIGHDPI ]
        drawRange :: HasCallStack => ArrayRange -> IO ()
        drawRange (ArrayRange mode from count) = checkingGLError $ glDrawArrays mode (fromIntegral from) (fromIntegral count)
        jitterPattern
          = [ V2 (-1 / 12.0) (-5 / 12.0)
            , V2 ( 1 / 12.0) ( 1 / 12.0)
            , V2 ( 3 / 12.0) (-1 / 12.0)
            , V2 ( 5 / 12.0) ( 5 / 12.0)
            , V2 ( 7 / 12.0) (-3 / 12.0)
            , V2 ( 9 / 12.0) ( 3 / 12.0)
            ]
        windowSize = V2 width height
        width  = 1024
        height = 768

infixl 5 `translate`

translate :: M33 Float -> V2 Float -> M33 Float
translate (V3 r1 r2 r3) (V2 tx ty) = V3 (over _z (+ tx) r1) (over _z (+ ty) r2) r3

(^.) :: s -> ((a -> Const a a) -> s -> Const a s) -> a
(^.) s l = getConst (l Const s)

set' :: s -> ((a -> Identity a) -> s -> Identity s) -> a -> s
set' s l a = runIdentity (l (const Identity a) s)

type ASetter s t a b = (a -> Identity b) -> s -> Identity t

over :: ASetter s t a b -> (a -> b) -> s -> t
over l f = runIdentity . l (Identity . f)

data Instance = Instance
  { instanceGlyph  :: {-# UNPACK #-} !Glyph
  , instanceOffset :: {-# UNPACK #-} !(V2 Float)
  , instanceBounds :: {-# UNPACK #-} !(Rect Float)
  , instanceScale  :: {-# UNPACK #-} !(V2 Float)
  }

minX :: Rect a -> a
minX = (^. _x) . intervalMin

minY :: Rect a -> a
minY = (^. _y) . intervalMin

maxX :: Rect a -> a
maxX = (^. _x) . intervalMax

maxY :: Rect a -> a
maxY = (^. _y) . intervalMax

combineInstances :: V2 Float -> [Instance] -> Glyph -> [Instance]
combineInstances scale instances@(Instance g (V2 x y) _ _:_) glyph = instances <> [ Instance glyph (V2 (x + glyphAdvanceWidth g) y) (scaleInterval scale (translateInterval (V2 (x + glyphAdvanceWidth g) y) (glyphBounds glyph))) scale ]
combineInstances scale [] glyph = [ Instance glyph (V2 0 0) (glyphBounds glyph) scale ]

instanceGeometry :: Instance -> [V4 Float]
instanceGeometry Instance{..} = glyphGeometry instanceGlyph

data Glyph = Glyph
  { glyphCodePoint    :: !Char
  , glyphAdvanceWidth :: {-# UNPACK #-} !Float
  , glyphGeometry     :: ![V4 Float]
  , glyphBounds       :: {-# UNPACK #-} !(Rect Float)
  }

scaleGlyph :: V2 Float -> Glyph -> Glyph
scaleGlyph (V2 sx sy) Glyph{..} = Glyph glyphCodePoint (glyphAdvanceWidth * sx) ((* V4 sx sy 1 1) <$> glyphGeometry) (scaleInterval (V2 sx sy) glyphBounds)

scaleInterval :: Num a => a -> Interval a -> Interval a
scaleInterval scale Interval{..} = Interval (intervalMin * scale) (intervalMax * scale)

translateInterval :: Num a => a -> Interval a -> Interval a
translateInterval delta Interval{..} = Interval (intervalMin + delta) (intervalMax + delta)

sfoldMap :: (Foldable f, Semigroup s) => (a -> s) -> f a -> Maybe s
sfoldMap f = getOption . foldMap (Option . Just . f)

setClearColour :: Linear.V4 Float -> IO ()
setClearColour (V4 r g b a) = glClearColor r g b a

combineGeometry :: ArrayVertices (v n) -> Geometry (v n) -> ArrayVertices (v n)
combineGeometry ArrayVertices{..} (Geometry mode vertices) =
  let count = length vertices
  in ArrayVertices
    (arrayVertices <> vertices)
    (prevIndex + count)
    (arrayRanges <> [ ArrayRange { mode = mode, firstVertexIndex = prevIndex, vertexCount = count } ])

data Typeface = Typeface { typefaceName :: String, typefaceUnderlying :: O.OpentypeFont }

data Font = Font { fontFace :: Typeface, fontSize :: Float }

readTypeface :: FilePath -> IO (Maybe Typeface)
readTypeface path = (toTypeface <$> O.readOTFile path) `E.catch` (\ (E.SomeException _) -> return Nothing)
  where toTypeface font = do
          name <- opentypeFontName font
          pure $ Typeface name font

data NameID
  = Copyright
  | FamilyName
  | SubfamilyName
  | UniqueID
  | FullName
  | Version
  | PostScriptName
  | Trademark
  | ManufacturerName
  | Designer
  | Description
  | VendorURL
  | DesignerURL
  | LicenseDescription
  | LicenseURL
  | Reserved
  | TypographicFamilyName
  | TypographicSubfamilyName
  | CompatibleFullName
  | SampleText
  | PostScriptCIDFindFontName
  | WWSFamilyName
  | WWSSubfamilyName
  | LightBackgroundPalette
  | DarkBackgroundPalette
  | VariationsPostScriptNamePrefix
  deriving (Bounded, Enum, Eq, Ord, Show)

opentypeFontName :: O.OpentypeFont -> Maybe String
opentypeFontName o = T.unpack . T.decodeUtf16BE . O.nameString <$> find ((== Just FullName) . nameID) (O.nameRecords (O.nameTable o))

nameID :: O.NameRecord -> Maybe NameID
nameID = safeToEnum . fromIntegral . O.nameID

glyphs :: Typeface -> [Char] -> [Glyph]
glyphs typeface chars = concat (zipWith toGlyph chars (glyphsForChars typeface chars))
  where toGlyph char (Just g) = let vertices = glyphVertices typeface g in
          [ scaleGlyph (V2 (1 / fromIntegral (unitsPerEm typeface)) (1 / fromIntegral (unitsPerEm typeface))) $ Glyph char (fromIntegral (O.advanceWidth g)) vertices (bounds ((^. _xy) <$> vertices)) ]
        toGlyph _ Nothing = []
        bounds vertices = Interval (foldr1 (liftA2 min) vertices) (foldr1 (liftA2 max) vertices)

glyphsForChars :: Typeface -> [Char] -> [Maybe (O.Glyph Int)]
glyphsForChars (Typeface _ o) chars = map (>>= (glyphs !?) . fromIntegral) glyphIDs
  where glyphIDs = fromMaybe (Nothing <$ chars) $ do
          cmap <- find viablePlatform (O.getCmaps (O.cmapTable o))
          Just $ lookupAll (O.glyphMap cmap) (fmap (fromIntegral . ord :: Char -> Word32) chars)
        lookupAll = fmap . flip Map.lookup
        glyphs = case O.outlineTables o of
          O.QuadTables _ (O.GlyfTable glyphs) -> glyphs
          _ -> error "wtf"
        viablePlatform p = O.cmapPlatform p == O.UnicodePlatform || O.cmapPlatform p == O.MicrosoftPlatform && O.cmapEncoding p == 1

unitsPerEm :: Typeface -> Word16
unitsPerEm = O.unitsPerEm . O.headTable . typefaceUnderlying

ascent :: Typeface -> Int16
ascent = O.ascent . O.hheaTable . typefaceUnderlying

descent :: Typeface -> Int16
descent = O.descent . O.hheaTable . typefaceUnderlying

safeToEnum :: forall n. (Bounded n, Enum n) => Int -> Maybe n
safeToEnum n = do
  guard (n < fromEnum (maxBound :: n))
  guard (n > fromEnum (minBound :: n))
  pure (toEnum n)

data Path v n
  = M (v n) (Path v n)
  | L (v n) (Path v n)
  | Q (v n) (v n) (Path v n)
  | Z
  deriving (Eq, Functor, Show)

contourToPath :: [O.CurvePoint] -> Path V2 O.FWord
contourToPath [] = Z
contourToPath (p@(O.CurvePoint x y _) : rest) = makePath Z
  where (makePath, _) = (foldl (\ (makePath, prev) point -> (makePath . pathFor prev point, point)) (M (V2 x y), p) rest)
        pathFor (O.CurvePoint _ _ True)  (O.CurvePoint _ _ False)   = id
        pathFor (O.CurvePoint _ _ True)  (O.CurvePoint x y True)    = L (V2 x y)
        pathFor (O.CurvePoint x y False) (O.CurvePoint x' y' False) = Q (V2 x y) (V2 (x + ((x' - x) `div` 2)) (y + ((y' - y) `div` 2)))
        pathFor (O.CurvePoint x y False) (O.CurvePoint x' y' True)  = Q (V2 x y) (V2 x' y')

glyphPaths :: Typeface -> O.Glyph Int -> [Path V2 O.FWord]
glyphPaths typeface glyph = fmap contourToPath (O.getScaledContours (typefaceUnderlying typeface) glyph)


pathTriangles :: (Int, V2 O.FWord, V2 O.FWord) -> Path V2 O.FWord -> [(Triangle V2 O.FWord, Bool)]
pathTriangles (count, first, current) p = case p of
  M v rest ->                                                                             pathTriangles (0,          v,     v ) rest
  L v rest
    | count >= 2 -> (Triangle first current v,  True)                                   : pathTriangles (succ count, first, v ) rest
    | otherwise  ->                                                                       pathTriangles (succ count, first, v ) rest
  Q v1 v2 rest
    | count >= 2 -> (Triangle first current v2, True) : (Triangle current v1 v2, False) : pathTriangles (succ count, first, v2) rest
    | otherwise  ->                                     (Triangle current v1 v2, False) : pathTriangles (succ count, first, v2) rest
  Z ->                                                                                    []

triangleVertices :: Triangle V2 O.FWord -> Bool -> [V4 Float]
triangleVertices (Triangle (V2 ax ay) (V2 bx by) (V2 cx cy)) True  = [ V4 (fromIntegral ax) (fromIntegral ay) 0 1, V4 (fromIntegral bx) (fromIntegral by) 0 1, V4 (fromIntegral cx) (fromIntegral cy) 0 1 ]
triangleVertices (Triangle (V2 ax ay) (V2 bx by) (V2 cx cy)) False = [ V4 (fromIntegral ax) (fromIntegral ay) 0 0, V4 (fromIntegral bx) (fromIntegral by) 0.5 0, V4 (fromIntegral cx) (fromIntegral cy) 1 1 ]

glyphVertices :: Typeface -> O.Glyph Int -> [V4 Float]
glyphVertices typeface = (>>= uncurry triangleVertices) . (>>= pathTriangles (0, V2 0 0, V2 0 0)) . glyphPaths typeface


encodePath :: Path V2 O.FWord -> [Word8]
encodePath = go . fmap (word16Bytes . fromIntegral)
  where go path = case path of
          M (V2 x y)            rest -> moveTo  : x ++ y             ++ go rest
          L (V2 x y)            rest -> lineTo  : x ++ y             ++ go rest
          Q (V2 x y) (V2 x' y') rest -> curveTo : x ++ y ++ x' ++ y' ++ go rest
          _                          -> close   : []
        (moveTo, lineTo, curveTo, close) = (0, 1, 2, 3)

word16Bytes :: Word16 -> [Word8]
word16Bytes x = [ fromIntegral $ x .&. 0xFF, fromIntegral $ (x .&. 0xFF00) `shiftR` 8 ]

word32Bytes :: Word32 -> [Word8]
word32Bytes x = [ fromIntegral $ x .&. 0xFF, fromIntegral $ (x .&. 0xFF00) `shiftR` 8, fromIntegral $ (x .&. 0xFF0000) `shiftR` 16, fromIntegral $ (x .&. 0xFF000000) `shiftR` 24 ]


encodeGlyphPaths :: Typeface -> O.Glyph Int -> [Word8]
encodeGlyphPaths typeface = (>>= encodePath) . glyphPaths typeface


encodeGlyphsForChars :: Typeface -> [Char] -> [Word8]
encodeGlyphsForChars face chars = header ++ glyphHeaders ++ (charsGlyphsAndPaths >>= \ (_, _, path) -> path)
  where charsGlyphsAndPaths = zip chars (glyphsForChars face chars) >>= \ (char, glyph) -> (,,) char <$> toList glyph <*> fmap (encodeGlyphPaths face) (toList glyph)
        header = word16Bytes (unitsPerEm face) ++ word16Bytes (fromIntegral (ascent face)) ++ word16Bytes (fromIntegral (descent face)) ++ word16Bytes (fromIntegral (length charsGlyphsAndPaths))
        glyphHeaders = snd (foldl encodeGlyphHeader (0, id) charsGlyphsAndPaths) []
        encodeGlyphHeader (offset, makeList) (char, glyph, path) = (offset + fromIntegral (length path), makeList . (++ (word16Bytes (fromIntegral (ord char)) ++ word16Bytes (O.advanceWidth glyph) ++ word32Bytes offset ++ word16Bytes (fromIntegral (length path)))))

ignoreEventsOfTypes :: [Word32] -> IO ()
ignoreEventsOfTypes = traverse_ (\ t -> SDL.eventState t 0 >>= checkWhen (/= 0))

withWindow :: C.CString -> Linear.V2 Int -> Word32 -> (SDL.Window -> IO a) -> IO a
withWindow name (V2 w h) flags = E.bracket
  (SDL.createWindow name SDL.SDL_WINDOWPOS_CENTERED SDL.SDL_WINDOWPOS_CENTERED (fromIntegral w) (fromIntegral h) flags >>= checkNonNull)
  SDL.destroyWindow

withContext :: SDL.Window -> (SDL.GLContext -> IO a) -> IO a
withContext window = E.bracket
  (SDL.glCreateContext window >>= checkNonNull)
  SDL.glDeleteContext

checkWhen :: (a -> Bool) -> a -> IO a
checkWhen predicate value = do
  when (predicate value) checkSDLError
  pure value

checkNonNull :: Ptr a -> IO (Ptr a)
checkNonNull = checkWhen (== nullPtr)

checkSDLError :: IO ()
checkSDLError = do
  msg <- SDL.getError >>= C.peekCString
  SDL.clearError
  when (msg /= "") $ E.throw $ SDLException msg

set :: SDL.GLattr -> Int -> IO ()
set attribute value = do
  result <- SDL.glSetAttribute attribute (fromIntegral value)
  _ <- checkWhen (< 0) result
  pure ()

newtype SDLException = SDLException String
  deriving (Show)

instance E.Exception SDLException

data Triangle v n = Triangle (v n) (v n) (v n)

data ArrayRange = ArrayRange { mode :: GLuint, firstVertexIndex :: Int, vertexCount :: Int }

data GeometryArray n = GeometryArray { geometryRanges :: [ArrayRange], geometryArray :: Array n }

data Interval a = Interval { intervalMin, intervalMax :: !a }
  deriving Show
type Rect a = Interval (V2 a)

instance (Applicative f, Ord a) => Semigroup (Interval (f a)) where
  Interval min1 max1 <> Interval min2 max2 = Interval (min <$> min1 <*> min2) (max <$> max1 <*> max2)

data ArrayVertices a = ArrayVertices { arrayVertices :: [a], prevIndex :: Int, arrayRanges :: [ArrayRange] }

data Geometry a where
  Geometry :: (Foldable v, Scalar n) => GLuint -> [v n] -> Geometry (v n)
