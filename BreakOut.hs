{-# LANGUAGE PackageImports #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
module Main where

import qualified App as App
import qualified Mesh as Mesh
import qualified Shader as Shader
import qualified BarMesh as BarMesh
import LightSource

import qualified "GLFW-b" Graphics.UI.GLFW as GLFW
import Graphics.Rendering.OpenGL.Raw
import Graphics.Rendering.OpenGL.GL.ByteString
import Data.Maybe (isNothing, fromJust)
import Data.Bits
import Data.IORef
import Data.Array.Storable
import Data.Vec as V
import Control.Monad
import Control.Exception
import Control.Concurrent (threadDelay)
import System.Environment
import System.Exit
import System.IO
import Foreign.C.Types
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.Storable
import Codec.Image.PNG

vec4 = Mesh.vec4

-- | Actor is the mesh object controller.
data Actor = Actor
  { object :: Mesh.Object
  , matrix :: V.Mat44 GLfloat
  , position :: Vec3 GLfloat
  }

-- | the light source list.
lightSource :: [LightSource]
lightSource =
  [ LightSource
      { diffuse =  vec4 1000000 1000000 1000000 1
      , specular = vec4  200000  500000  700000 1
      , LightSource.position = vec4 (-300) 300 (-300) 1
      }
  , LightSource
      { diffuse =  vec4 0 0 1000000 1
      , specular = vec4  20000  10000  10000 1
      , LightSource.position = vec4 300 (-300) 300 1
      }
  , LightSource
      { diffuse =  vec4 1000000 1000000 1000000 1
      , specular = vec4  500000  700000  900000 1
      , LightSource.position = vec4 (-300) 300 (-300) 0
      }
  , LightSource
      { diffuse =  vec4 1000000 1000000 1000000 1
      , specular = vec4  500000  700000  900000 1
      , LightSource.position = vec4 (-300) 300 (-300) 0
      }
  ]

-- | Transform the light source position in the view coordinates.
toViewSpace :: Mat44 GLfloat -> LightSource -> LightSource
toViewSpace m ls = ls { LightSource.position = V.multmv m (LightSource.position ls) }

-- | the material list for any mesh object.
materials :: [Material]
materials =
  [ Material
    { baseColor = Mesh.vec4 0.8 0.5 0.1 1
    , metallic = 0.5
    , roughness = 0.1
    }
  , Material
    { baseColor = Mesh.vec4 0.6 0.5 0.4 1
    , metallic = 0.5
    , roughness = 0.1
    }
  , Material
    { baseColor = Mesh.vec4 0.8 0.2 0.3 1
    , metallic = 0.3
    , roughness = 0.6
    }
  , Material
    { baseColor = Mesh.vec4 0.3 0.9 0.2 1
    , metallic = 0.1
    , roughness = 0.6
    }
  , Material
    { baseColor = Mesh.vec4 0.2 0.6 0.5 1
    , metallic = 0.7
    , roughness = 0.1
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

rgb8 = 0x8051 :: GLint
rgba8 :: GLint
rgba8 = 0x8058 :: GLint
unsignedByte = 0x1401 :: GLenum
textureWrapS = 0x2802 :: GLenum
textureWrapT = 0x2803 :: GLenum

newtype TextureObject = TextureObject { textureID :: GLuint }

loadTexture :: FilePath -> IO (Either String TextureObject)
loadTexture path = do
  x <- loadPNGFile path
  case x of
    Left e -> return $ Left e
    Right img -> do
      ptr <- alloca $ \texture -> do
        glGenTextures 1 texture
        texId <- peek texture
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
        return texture;
      texId <- peek ptr
      return . Right $ TextureObject texId

unloadTexture :: TextureObject -> IO ()
unloadTexture texture = do
  withArrayLen [(textureID texture)] $ glDeleteTextures . fromIntegral

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
  }

-- | The main loop in the game.
renderingLoop :: GLFW.Window -> [Actor] -> IO ()
renderingLoop window initialActors = do
  GLFW.setCursorInputMode window GLFW.CursorInputMode'Disabled
  curPos <- GLFW.getCursorPos window
  gameData <- newIORef $ Main.GameData
    { camera = Camera
        { pos = vec3 (125) (-100) (-450)
        , target = vec3 (125) (150) (0)
        , up = vec3 0 1 0
        , shininess = 64
        }
    , cur = curPos
    , ballSpeed = (2, 2)
    }
  loop initialActors gameData
  where
    keyAction key taction faction = do
      keyState <- GLFW.getKey window key
      if (keyState == GLFW.KeyState'Pressed) then taction else faction

    loop actors gameData = (GLFW.windowShouldClose window) >>= (flip unless) (go actors gameData)
    go actors gameData = do
      gd <- readIORef gameData
      display (camera gd) actors
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

    boundWall pos speed top bottom =
      let n = pos + speed
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
lookAt eye target up = x :. y :. z :. h :. ()
  where
    forward = V.normalize $ target - eye
    right = V.normalize $ V.cross forward up
    up' = V.cross right forward
    x = V.snoc right (-(V.dot right eye))
    y = V.snoc up' (-(V.dot up' eye))
    z = V.snoc (-forward) (V.dot forward eye)
    h = 0 :. 0 :. 0 :. 1 :. ()

-- | Render all of the actors.
display :: Camera -> [Actor] -> IO ()
display camera [] = return ()
display camera actors = do
  glClearColor 0.1 0.4 0.2 1
  glClear $ gl_COLOR_BUFFER_BIT .|. gl_DEPTH_BUFFER_BIT

  let viewMatrix = Main.lookAt (pos camera) (target camera) (up camera)
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

  drawMesh viewMatrix projMatrix actors
  glFlush
  where
    drawMesh :: Mat44 GLfloat -> Mat44 GLfloat -> [Actor] -> IO ()
    drawMesh _ _ [] = return ()
    drawMesh viewMatrix projMatrix (Actor mesh mat pos:xs) = do
      Mesh.draw mesh (V.translate pos mat) viewMatrix projMatrix
      drawMesh viewMatrix projMatrix xs
