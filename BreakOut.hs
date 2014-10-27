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
import Data.Vec as V
import Control.Monad
import Control.Concurrent (threadDelay)
import System.Environment
import System.Exit
import Foreign.C.Types
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.Storable

vec4 = Mesh.vec4

data Actor = Actor
  { object :: Mesh.Object
  , matrix :: V.Mat44 GLfloat
  }

lightSource :: [LightSource]
lightSource =
  [ LightSource
      { diffuse =  vec4 1000000 1000000 1000000 1
      , specular = vec4  200000  500000  700000 1
      , position = vec4 (-300) 300 (-300) 1
      }
  , LightSource
      { diffuse =  vec4 0 0 1000000 1
      , specular = vec4  20000  10000  10000 1
      , position = vec4 300 (-300) 300 1
      }
  , LightSource
      { diffuse =  vec4 1000000 1000000 1000000 1
      , specular = vec4  500000  700000  900000 1
      , position = vec4 (-300) 300 (-300) 0
      }
  , LightSource
      { diffuse =  vec4 1000000 1000000 1000000 1
      , specular = vec4  500000  700000  900000 1
      , position = vec4 (-300) 300 (-300) 0
      }
  ]

toViewSpace :: Mat44 GLfloat -> LightSource -> LightSource
toViewSpace m ls = ls { LightSource.position = V.multmv m (LightSource.position ls) }

materials :: [Material]
materials =
  [ Material
    { baseColor = Mesh.vec4 0.8 0.5 0.1 1
    , metallic = 0.5
    , roughness = 0.1
    }
  , Material
    { baseColor = Mesh.vec4 0.2 0.6 0.5 1
    , metallic = 0.1
    , roughness = 0.6
    }
  ]

main :: IO ()
main = do
  progName <- getProgName
  r <- App.create progName 800 600
  when (isNothing r) exitFailure

  cubeMesh <- Mesh.create BarMesh.chamferedCubeVertices BarMesh.chamferedCubeIndices
  let identityMatrix = V.identity :: Mat44 CFloat
      meshA = cubeMesh { Mesh.material = materials !! 0 }
      meshB = cubeMesh { Mesh.material = materials !! 1 }
      meshC = cubeMesh { Mesh.material = materials !! 1 }
      mA = V.scale (vec4 10 10 10 1) identityMatrix
      mB = V.translate (vec3 300 0 0) $ V.scale (vec4 10 10 10 1) identityMatrix
      mC = V.translate (vec3 0 300 300) identityMatrix
      actors =
        [ Actor meshA mA
        , Actor meshB mB
        , Actor meshC mC
        ]
  renderingLoop (fromJust r) actors
  Mesh.destroy cubeMesh
  App.destroy (fromJust r)

data Camera = Camera
  { pos :: Vec3 GLfloat
  , target :: Vec3 GLfloat
  , up :: Vec3 GLfloat
  , cur :: (Double, Double) -- for mouse movement.
  , shininess :: GLfloat
  }

renderingLoop :: GLFW.Window -> [Actor] -> IO ()
renderingLoop window actors = do
  GLFW.setCursorInputMode window GLFW.CursorInputMode'Disabled
  curPos <- GLFW.getCursorPos window
  camera <- newIORef $ Main.Camera
    { pos = vec3 250 250 500
    , target = vec3 0 0 0
    , up = vec3 0 1 0
    , cur = curPos
    , shininess = 64
    }
  loop camera
  where
    keyAction key taction faction = do
      keyState <- GLFW.getKey window key
      if (keyState == GLFW.KeyState'Pressed) then taction else faction

    loop camera = (GLFW.windowShouldClose window) >>= (flip unless) (go camera)
    go camera = do
      display camera actors
      GLFW.swapBuffers window
      GLFW.pollEvents

      c <- readIORef camera
      let zeroVector = vec3 0 0 0
      let targetDistance = (target c) - (pos c)
          frontVector = (V.normalize targetDistance) * 2
          leftVector = (V.cross (up c) frontVector) * 2

      mw <- keyAction GLFW.Key'W (return frontVector) (return zeroVector)
      ma <- keyAction GLFW.Key'A (return leftVector) (return zeroVector)
      ms <- keyAction GLFW.Key'S (return (-frontVector)) (return zeroVector)
      md <- keyAction GLFW.Key'D (return (-leftVector)) (return zeroVector)

      su <- keyAction GLFW.Key'U (return (0.5 :: GLfloat)) (return 0)
      sd <- keyAction GLFW.Key'J (return (-0.5 :: GLfloat)) (return 0)

      (newX, newY) <- GLFW.getCursorPos window
      let (prevX, prevY) = cur c
          rotH = V.rotationVec (up c) $ realToFrac ((newX - prevX) * 0.001 * (-pi) )
          rotV = V.rotationVec leftVector $ realToFrac ((prevY - newY) * 0.001 * (-pi))
          newTarget = V.take n3 (V.multmv rotH (V.multmv rotV (V.snoc targetDistance 1)))

      let movement = mw + ma + ms + md
      writeIORef camera $ c
        { pos = (pos c) + movement
        , target = newTarget + (pos c) + movement
        , cur = (newX, newY)
        , shininess = (shininess c) + su + sd
        }
      isExit <- GLFW.getKey window GLFW.Key'Escape
      when (isExit /= GLFW.KeyState'Pressed) $ do
        threadDelay 10000
        loop camera

vec3 :: forall a a1 a2. a -> a1 -> a2 -> a :. (a1 :. (a2 :. ()))
vec3 x y z = x :. y :. z :. ()

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

display :: IORef Camera -> [Actor] -> IO ()
display camera [] = return ()
display camera actors = do
  glClearColor 0.1 0.4 0.2 1
  glClear $ gl_COLOR_BUFFER_BIT .|. gl_DEPTH_BUFFER_BIT

  c <- readIORef camera
  let viewMatrix = Main.lookAt (pos c) (target c) (up c)
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
    drawMesh viewMatrix projMatrix (Actor mesh mat:xs) = do
      Mesh.draw mesh mat viewMatrix projMatrix
      drawMesh viewMatrix projMatrix xs
