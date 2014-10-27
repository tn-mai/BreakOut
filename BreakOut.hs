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

  renderingLoop (fromJust r) $ paddleActor ++ ballActor ++ blockActors ++ floorActors ++ wallActors ++ sideWallActors
  Mesh.destroy cubeMesh
  App.destroy (fromJust r)
  where
    makeActors mesh mat posList =
      Prelude.map
        (\(x, y, z) -> Actor mesh mat (vec3 x y z))
        posList

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

{--
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
        }
--}
      (newX, newY) <- GLFW.getCursorPos window
      let (prevX, _) = cur gd
          delta = realToFrac ((newX - prevX) * (-1)) :: GLfloat
          paddle = actors !! 0
          (x :. y :. z :. ()) = Main.position paddle
          newPaddleX = max 20 . min 230 $ (x + delta)
          newActors = paddle { Main.position = vec3 newPaddleX y z  } : (Prelude.drop 1 actors)

      writeIORef gameData $ gd { cur = (newX, newY) }

      isExit <- GLFW.getKey window GLFW.Key'Escape
      when (isExit /= GLFW.KeyState'Pressed) $ do
        threadDelay 10000
        loop newActors gameData

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
