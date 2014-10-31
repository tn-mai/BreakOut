{-# LANGUAGE PackageImports #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
module Main where

import qualified App as App
import qualified Mesh as Mesh
import Mesh (vec4)
import qualified Shader as Shader
import qualified BarMesh as BarMesh
import qualified LightSource as LS

import qualified "GLFW-b" Graphics.UI.GLFW as GLFW
import Graphics.Rendering.OpenGL hiding (TextureObject, Line)
import Graphics.Rendering.OpenGL.Raw
import Graphics.Rendering.OpenGL.GL.ByteString
import Data.Maybe (isNothing, fromJust)
import Data.Bits
import Data.IORef
import Data.Array.Storable
import Data.Vec as V
import Control.Monad
import Control.Concurrent (threadDelay)
import System.Environment
import System.Exit
import System.IO
import Foreign.C.Types
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.Storable
import Codec.Image.PNG

-- | Actor is the mesh object controller.
data Actor = Actor
  { object :: Mesh.Object
  , matrix :: V.Mat44 GLfloat
  , position :: Vec3 GLfloat
  }

-- | the light source list.
lightSource :: [LS.LightSource]
lightSource =
  [ LS.LightSource
      { LS.diffuse =  vec4 1000000 1000000 1000000 1
      , LS.specular = vec4  200000  500000  700000 1
      , LS.position = vec4 (-300) 300 (-300) 1
      }
  , LS.LightSource
      { LS.diffuse =  vec4 0 0 1000000 1
      , LS.specular = vec4  20000  10000  10000 1
      , LS.position = vec4 300 (-300) 300 1
      }
  , LS.LightSource
      { LS.diffuse =  vec4 1000000 1000000 1000000 1
      , LS.specular = vec4  500000  700000  900000 1
      , LS.position = vec4 (-300) 300 (-300) 0
      }
  , LS.LightSource
      { LS.diffuse =  vec4 1000000 1000000 1000000 1
      , LS.specular = vec4  500000  700000  900000 1
      , LS.position = vec4 (-300) 300 (-300) 0
      }
  ]

-- | Transform the light source position in the view coordinates.
toViewSpace :: Mat44 GLfloat -> LS.LightSource -> LS.LightSource
toViewSpace m ls = ls { LS.position = V.multmv m (LS.position ls) }

-- | the material list for any mesh object.
materials :: [LS.Material]
materials =
  [ LS.Material
    { LS.baseColor = Mesh.vec4 0.8 0.5 0.1 1
    , LS.metallic = 0.5
    , LS.roughness = 0.1
    }
  , LS.Material
    { LS.baseColor = Mesh.vec4 0.6 0.5 0.4 1
    , LS.metallic = 0.5
    , LS.roughness = 0.1
    }
  , LS.Material
    { LS.baseColor = Mesh.vec4 0.8 0.2 0.3 1
    , LS.metallic = 0.3
    , LS.roughness = 0.6
    }
  , LS.Material
    { LS.baseColor = Mesh.vec4 0.3 0.9 0.2 1
    , LS.metallic = 0.1
    , LS.roughness = 0.6
    }
  , LS.Material
    { LS.baseColor = Mesh.vec4 0.2 0.6 0.5 1
    , LS.metallic = 0.7
    , LS.roughness = 0.1
    }
  ]

-- | The entry point.
main :: IO ()
main = do
  progName <- getProgName
  r <- App.create progName 800 600
  when (isNothing r) exitFailure

  cubeMesh <- Mesh.create BarMesh.chamferedCubeVertices BarMesh.chamferedCubeIndices
  let identityMatrix = V.identity :: Mat44 CFloat
      floorActors = makeActors
        cubeMesh { Mesh.material = materials !! 0 }
        (V.scale (vec4 2.5 2.5 0.5 1) identityMatrix)
        $ [(x, y, 0) | x <- [0, 50..250], y <- [0, 50 .. 350]]
      wallActors = makeActors
        cubeMesh { Mesh.material = materials !! 1 }
        (V.scale (vec4 2.5 0.5 2.5 1) identityMatrix)
        $ [(x, y, (-20)) | x <- [0, 50..250], y <- [-30, 380]]
      sideWallActors = makeActors
        cubeMesh { Mesh.material = materials !! 1 }
        (V.scale (vec4 0.5 2.5 2.5 1) identityMatrix)
        $ [(x, y, (-20)) | x <- [-30, 280], y <- [0, 50 .. 350]]
      paddleActor = makeActors
        cubeMesh { Mesh.material = materials !! 2 }
        (V.scale (vec4 4.0 1.0 1.0 1) identityMatrix)
        $ [(125, 50, -30)]
      blockActors = makeActors
        cubeMesh { Mesh.material = materials !! 3 }
        (V.scale (vec4 2.5 1.25 1.25 1) identityMatrix)
        $ [(x, y, (-30)) | x <- [25, 75 .. 200], y <- [200, 225 .. 300]]
      ballActor = makeActors
        cubeMesh { Mesh.material = materials !! 4 }
        (V.scale (vec4 0.5 0.5 0.5 1) identityMatrix)
        $ [(125, 100, -30)]

  renderingLoop (fromJust r) $ paddleActor ++ ballActor ++ floorActors ++ wallActors ++ sideWallActors ++ blockActors
  Mesh.destroy cubeMesh
  App.destroy (fromJust r)
  where
    makeActors mesh mat posList =
      Prelude.map
        (\(x, y, z) -> Actor mesh mat (vec3 x y z))
        posList

rgb8 :: GLint
rgb8 = 0x8051
rgba8 :: GLint
rgba8 = 0x8058 :: GLint
unsignedByte :: GLenum
unsignedByte = 0x1401 :: GLenum
textureWrapS :: GLenum
textureWrapS = 0x2802 :: GLenum
textureWrapT :: GLenum
textureWrapT = 0x2803 :: GLenum

newtype TextureObject = TextureObject { textureID :: GLuint }

loadTexture :: FilePath -> IO (Either String Main.TextureObject)
loadTexture path = do
  x <- loadPNGFile path
  case x of
    Left e -> return $ Left e
    Right img -> do
      texId <- alloca $ \texIdPtr -> do
        glGenTextures 1 texIdPtr
        texId <- peek texIdPtr
        glBindTexture gl_TEXTURE_2D texId
        glTexParameteri gl_TEXTURE_2D gl_TEXTURE_MIN_FILTER (fromIntegral gl_LINEAR)
        glTexParameteri gl_TEXTURE_2D gl_TEXTURE_MAG_FILTER (fromIntegral gl_LINEAR)
        glTexParameteri gl_TEXTURE_2D textureWrapS (fromIntegral gl_CLAMP_TO_EDGE)
        glTexParameteri gl_TEXTURE_2D textureWrapT (fromIntegral gl_CLAMP_TO_EDGE)
        let (w, h) = dimensions img
            (pif, pf) = if hasAlphaChannel img then (rgba8, gl_RGBA) else (rgb8, gl_RGB)
        withStorableArray (imageData img) $ \ptr ->
          glTexImage2D gl_TEXTURE_2D 0 pif (fromIntegral w) (fromIntegral h) 0 pf unsignedByte ptr
        glBindTexture gl_TEXTURE_2D 0
        return texId;
      return . Right $ Main.TextureObject texId

unloadTexture :: Main.TextureObject -> IO ()
unloadTexture tex = do
  withArrayLen [(textureID tex)] $ glDeleteTextures . fromIntegral

textureUnit :: GLuint -> GLenum
textureUnit x = 0x84c0 + x

-- | The camera object.
data Camera = Camera
  { pos :: Vec3 GLfloat
  , target :: Vec3 GLfloat
  , up :: Vec3 GLfloat
  , shininess :: GLfloat
  }

data GameData = GameData
  { camera :: Camera
  , cur :: (Double, Double) -- for mouse movement.
  , ballSpeed :: (GLfloat, GLfloat)
  , asciiShader :: Shader.Program
  , asciiTexLocation :: Shader.UniformLocation
  , asciiTex :: Main.TextureObject
  }

-- | The main loop in the game.
renderingLoop :: GLFW.Window -> [Actor] -> IO ()
renderingLoop window initialActors = do
  GLFW.setCursorInputMode window GLFW.CursorInputMode'Disabled
  curPos <- GLFW.getCursorPos window

  asciiProgram <- Shader.load 
    [ Shader.Info gl_VERTEX_SHADER (Shader.FileSource "shaders/bitmapfont.vert")
    , Shader.Info gl_FRAGMENT_SHADER (Shader.FileSource "shaders/bitmapfont.frag")
    ]
  asciiTexLoc <- Shader.uniformLocation asciiProgram "texture"

  eitherTex <- loadTexture "data/ascii.png"
  case eitherTex of
    Left e -> hPutStrLn stderr e
    Right _ -> hPutStrLn stderr "success loading texture."
  let (Right tex) = eitherTex

  gameData <- newIORef $ Main.GameData
    { camera = Camera
        { pos = vec3 (125) (-100) (-450)
        , target = vec3 (125) (150) (0)
        , up = vec3 0 1 0
        , shininess = 64
        }
    , cur = curPos
    , ballSpeed = (2, 2)
    , asciiShader = asciiProgram
    , asciiTexLocation = asciiTexLoc
    , asciiTex = tex
    }

  loop initialActors gameData
  where
    keyAction key taction faction = do
      keyState <- GLFW.getKey window key
      if (keyState == GLFW.KeyState'Pressed) then taction else faction

    loop actors gameData = (GLFW.windowShouldClose window) >>= (flip unless) (go actors gameData)
    go actors gameData = do
      gd <- readIORef gameData
      display gd actors
      GLFW.swapBuffers window
      GLFW.pollEvents

      (newX, newY) <- GLFW.getCursorPos window
      let (prevX, _) = cur gd
          delta = realToFrac ((newX - prevX) * (-1)) :: GLfloat
          paddle = actors !! 0
          (x :. y :. z :. ()) = Main.position paddle
          newPaddleX = max 20 . min 230 $ (x + delta)

      let ball = actors !! 1
          (speedX, speedY) = ballSpeed gd
          (bx :. by :. bz :. ()) = Main.position ball
          (newBallX, newSpeedX) = boundWall (bx + speedX) speedX 270 (-20)
          (newBallY, newSpeedY) = boundWall (by + speedY) speedY 370 (-20)
          (newBallX', newBallY', newSpeedX', newSpeedY') =
            if boundPaddle (newBallX, newBallY) (newSpeedX, newSpeedY) (x, y)
            then (newBallX, newBallY, newSpeedX, (-newSpeedY))
            else (newBallX, newBallY, newSpeedX, newSpeedY)
          (hitX, hitY, nonHitBlocks) =
            intersectBlock (Line bx by (bx + speedX * 3) (by + speedY * 3)) (Prelude.drop 78 actors)
          newActors =
            ( paddle { Main.position = vec3 newPaddleX y z  }
            : ball { Main.position = vec3 newBallX' newBallY' bz }
            : Prelude.take 76 (Prelude.drop 2 actors)
            ) ++ nonHitBlocks

      writeIORef gameData $ gd
        { cur = (newX, newY)
        , ballSpeed = (if hitX then (-newSpeedX') else newSpeedX', if hitY then (-newSpeedY') else newSpeedY')
        }

      isExit <- GLFW.getKey window GLFW.Key'Escape
      when (isExit /= GLFW.KeyState'Pressed) $ do
        threadDelay 10000
        loop newActors gameData

    boundWall ballPos speed top bottom =
      let n = ballPos + speed
      in
        if n >= top
        then (n - (n - top), (-speed))
        else
          if n <= bottom
          then (n + (bottom - n), (-speed))
          else (n, speed)

    boundPaddle (bx, by) (sx, sy) (px, py) =
      if (by < pTop) || ((by + sy) > pTop)
      then False
      else
        if (nx > (px + 50)) || (nx < (px - 50))
        then False
        else True
      where
        pTop = py + 10
        nx = bx + (sx * (by - py) / sy)

intersectBlock :: Line GLfloat -> [Actor] -> (Bool, Bool, [Actor])
intersectBlock _ [] = (False, False, [])
intersectBlock ballLine@(Line x0 y0 x1 y1) (blockActor : xs) =
  if ((y1 - y0) < 0 && intersect ballLine topLine) || ((y1 - y0) >= 0 && intersect ballLine bottomLine)
  then (hitX, True, nonHitBlocks)
  else
    if ((x1 - x0) >= 0 && intersect ballLine leftLine) || ((x1 - x0) < 0 && intersect ballLine rightLine)
    then (True, hitY, nonHitBlocks)
    else (hitX, hitY, blockActor : nonHitBlocks)
  where
    (bx :. by :. _ :. ()) = Main.position blockActor
    (halfW, halfH) = (25, 12.5)
    topLine    = Line (bx - halfW) (by + halfH) (bx + halfW) (by + halfH)
    rightLine  = Line (bx + halfW) (by + halfH) (bx + halfW) (by - halfH)
    bottomLine = Line (bx + halfW) (by - halfH) (bx - halfW) (by - halfH)
    leftLine   = Line (bx - halfW) (by - halfH) (bx - halfW) (by + halfH)
    (hitX, hitY, nonHitBlocks) = intersectBlock ballLine xs

data Line a = Line a a a a

intersect :: Line GLfloat -> Line GLfloat -> Bool
intersect (Line bx by cx cy) (Line px py qx qy) =
  if ((bx - cx) * (py - by) + (by - cy) * (bx - px)) * ((bx - cx) * (qy - by) + (by - cy) * (bx - qx)) < (0 :: GLfloat)
  then
    if ((px - qx) * (by - py) + (py - qy) * (px - bx)) * ((px - qx) * (cy - py) + (py - qy) * (px - cx)) < (0 :: GLfloat)
    then True
    else False
  else False

-- | Make the 3 coordinate vector.
vec3 :: forall a a1 a2. a -> a1 -> a2 -> a :. (a1 :. (a2 :. ()))
vec3 x y z = x :. y :. z :. ()

-- | Make the view matrix looking at any point.
lookAt :: Floating a => Vec3 a -> Vec3 a -> Vec3 a -> Mat44 a
lookAt eyePos targetPos upVec = x :. y :. z :. h :. ()
  where
    forward = V.normalize $ targetPos - eyePos
    right = V.normalize $ V.cross forward upVec
    up' = V.cross right forward
    x = V.snoc right (-(V.dot right eyePos))
    y = V.snoc up' (-(V.dot up' eyePos))
    z = V.snoc (-forward) (V.dot forward eyePos)
    h = 0 :. 0 :. 0 :. 1 :. ()

-- | Render all of the actors.
display :: GameData -> [Actor] -> IO ()
display _ [] = return ()
display gameData actors = do
  let cam = camera gameData
  glClearColor 0.1 0.4 0.2 1
  glClear $ gl_COLOR_BUFFER_BIT .|. gl_DEPTH_BUFFER_BIT

  let viewMatrix = Main.lookAt (pos cam) (target cam) (up cam)
      projMatrix = (V.perspective 0.1 2000 (pi / 4) (4 / 3)) :: Mat44 GLfloat

  let newLS = Prelude.map (toViewSpace viewMatrix) lightSource
      progId = Shader.programId . Mesh.program . object $ actors !! 0
  alloca $ \ptr -> do
    let bufferId = 7
    glGenBuffers 1 ptr
    buffer <- peek ptr
    glBindBuffer gl_UNIFORM_BUFFER buffer
    withArray newLS $ \ls -> do
      glBufferData gl_UNIFORM_BUFFER (fromIntegral (Prelude.sum $ Prelude.map sizeOf newLS)) ls gl_DYNAMIC_DRAW
    glBindBuffer gl_UNIFORM_BUFFER 0
    glBindBufferBase gl_UNIFORM_BUFFER bufferId buffer
    idx <- withGLstring "LightSourceBlock" $ glGetUniformBlockIndex progId
    glUniformBlockBinding progId idx bufferId

  -- drawMesh viewMatrix projMatrix actors
  drawAscii "abc"
  glFlush
  where
    drawMesh :: Mat44 GLfloat -> Mat44 GLfloat -> [Actor] -> IO ()
    drawMesh _ _ [] = return ()
    drawMesh viewMatrix projMatrix (Actor mesh mat actorPos:xs) = do
      Mesh.draw mesh (V.translate actorPos mat) viewMatrix projMatrix
      drawMesh viewMatrix projMatrix xs

      glActiveTexture $ textureUnit 0
      glBindTexture gl_TEXTURE_2D $ textureID $ asciiTex gameData
      glUniform1i (asciiTexLocation gameData) 0

    drawAscii :: String -> IO ()
    drawAscii str = do
      let vertices =
            [   0, 10, 0, 1, 1, 1, 1, 0, 100
            , 10, 10, 0, 1, 1, 1, 1, 100, 100
            ,   0,   0, 0, 1, 1, 1, 1, 0, 0
            , 10,   0, 0, 1, 1, 1, 1, 100, 0
            ] :: [GLfloat]
      vao <- genObjectName
      vb <- Mesh.createBuffer ArrayBuffer vertices
      let numPositionElements = 3
          numColorElements = 4
          numTexCoordElements = 2
          vPosition = AttribLocation 0
          vColor = AttribLocation 2
          vTexCoord = AttribLocation 3
          offsetPosition = 0
          offsetColor = offsetPosition + numPositionElements
          offsetTexCoord = offsetColor + numColorElements
          sizeElement = sizeOf $ Prelude.head vertices
          sizeVertex = fromIntegral $ sizeElement * (numPositionElements + numColorElements + numTexCoordElements)
      bindVertexArrayObject $= Just vao
      bindBuffer ArrayBuffer $= Just vb
      vertexAttribPointer vPosition $=
        ( ToFloat
        , VertexArrayDescriptor (fromIntegral numPositionElements) Float sizeVertex (Mesh.bufferOffset (offsetPosition * sizeElement))
        )
      vertexAttribArray vPosition $= Enabled
      vertexAttribPointer vColor $=
        ( ToFloat
        , VertexArrayDescriptor (fromIntegral numColorElements) Float sizeVertex (Mesh.bufferOffset (offsetColor * sizeElement))
        )
      vertexAttribArray vColor $= Enabled
      vertexAttribPointer vTexCoord $=
        ( ToFloat
        , VertexArrayDescriptor (fromIntegral numColorElements) Float sizeVertex (Mesh.bufferOffset (offsetColor * sizeElement))
        )
      vertexAttribArray vTexCoord $= Enabled
      bindVertexArrayObject $= Nothing

      Shader.useProgram $ Just (asciiShader gameData)
      bindVertexArrayObject $= Just vao

      drawArrays TriangleStrip 0 4

      bindVertexArrayObject $= Nothing
      Shader.useProgram Nothing
      deleteObjectName vb
      deleteObjectName vao
