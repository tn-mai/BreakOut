{-# LANGUAGE PackageImports #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
module Main where

import qualified App as App
import qualified Mesh as Mesh

import qualified "GLFW-b" Graphics.UI.GLFW as GLFW
import Graphics.Rendering.OpenGL.Raw
import Data.Maybe (isNothing, fromJust)
import Data.Bits
import Data.IORef
import Data.Vec as V
import Control.Monad
import Control.Concurrent (threadDelay)
import System.Environment
import System.Exit
import Foreign.C.Types

main :: IO ()
main = do
  progName <- getProgName
  r <- App.create progName 800 600
  when (isNothing r) exitFailure

  {--
       top
           back
        6---7
  left /|  /|
      / 5-/-4
     3---2 / right
     |/  |/
     0---1
  front
       bottom
  --}
  let vertices0 = 
        -- front
        [ (-0.2), (-0.2), ( 0.2), ( 0.0), ( 0.0), ( 1.0), 1.0, 1.0, 1.0, 1.0
        , ( 0.2), (-0.2), ( 0.2), ( 0.0), ( 0.0), ( 1.0), 1.0, 1.0, 1.0, 1.0
        , ( 0.2), ( 0.2), ( 0.2), ( 0.0), ( 0.0), ( 1.0), 1.0, 1.0, 1.0, 1.0
        , (-0.2), ( 0.2), ( 0.2), ( 0.0), ( 0.0), ( 1.0), 1.0, 1.0, 1.0, 1.0

        -- back
        , ( 0.2), (-0.2), (-0.2), ( 0.0), ( 0.0), (-1.0), 1.0, 1.0, 1.0, 1.0
        , (-0.2), (-0.2), (-0.2), ( 0.0), ( 0.0), (-1.0), 1.0, 1.0, 1.0, 1.0
        , (-0.2), ( 0.2), (-0.2), ( 0.0), ( 0.0), (-1.0), 1.0, 1.0, 1.0, 1.0
        , ( 0.2), ( 0.2), (-0.2), ( 0.0), ( 0.0), (-1.0), 1.0, 1.0, 1.0, 1.0

        -- right
        , ( 0.2), (-0.2), ( 0.2), ( 1.0), ( 0.0), ( 0.0), 1.0, 1.0, 1.0, 1.0
        , ( 0.2), (-0.2), (-0.2), ( 1.0), ( 0.0), ( 0.0), 1.0, 1.0, 1.0, 1.0
        , ( 0.2), ( 0.2), (-0.2), ( 1.0), ( 0.0), ( 0.0), 1.0, 1.0, 1.0, 1.0
        , ( 0.2), ( 0.2), ( 0.2), ( 1.0), ( 0.0), ( 0.0), 1.0, 1.0, 1.0, 1.0

        -- left
        , (-0.2), (-0.2), (-0.2), (-1.0), ( 0.0), ( 0.0), 1.0, 1.0, 1.0, 1.0
        , (-0.2), (-0.2), ( 0.2), (-1.0), ( 0.0), ( 0.0), 1.0, 1.0, 1.0, 1.0
        , (-0.2), ( 0.2), ( 0.2), (-1.0), ( 0.0), ( 0.0), 1.0, 1.0, 1.0, 1.0
        , (-0.2), ( 0.2), (-0.2), (-1.0), ( 0.0), ( 0.0), 1.0, 1.0, 1.0, 1.0

        -- top
        , (-0.2), ( 0.2), ( 0.2), ( 0.0), ( 1.0), ( 0.0), 1.0, 1.0, 1.0, 1.0
        , ( 0.2), ( 0.2), ( 0.2), ( 0.0), ( 1.0), ( 0.0), 1.0, 1.0, 1.0, 1.0
        , ( 0.2), ( 0.2), (-0.2), ( 0.0), ( 1.0), ( 0.0), 1.0, 1.0, 1.0, 1.0
        , (-0.2), ( 0.2), (-0.2), ( 0.0), ( 1.0), ( 0.0), 1.0, 1.0, 1.0, 1.0

        -- bottom
        , (-0.2), (-0.2), (-0.2), ( 0.0), (-1.0), ( 0.0), 1.0, 1.0, 1.0, 1.0
        , ( 0.2), (-0.2), (-0.2), ( 0.0), (-1.0), ( 0.0), 1.0, 1.0, 1.0, 1.0
        , ( 0.2), (-0.2), ( 0.2), ( 0.0), (-1.0), ( 0.0), 1.0, 1.0, 1.0, 1.0
        , (-0.2), (-0.2), ( 0.2), ( 0.0), (-1.0), ( 0.0), 1.0, 1.0, 1.0, 1.0
        ] :: [GLfloat]
  let indices0 =
        [ 2, 1, 0, 0, 3, 2
        , 6, 5, 4, 4, 7, 6
        ,10, 9, 8, 8,11,10
        ,14,13,12,12,15,14
        ,18,17,16,16,19,18
        ,22,21,20,20,23,22
        ] :: [GLushort]
  mesh0 <- Mesh.create (Prelude.map (*20) vertices0) indices0
  renderingLoop (fromJust r) mesh0
  Mesh.destroy mesh0
  App.destroy (fromJust r)

data Camera = Camera
  { pos :: Vec3 GLfloat
  , target :: Vec3 GLfloat
  , up :: Vec3 GLfloat
  , cur :: (Double, Double) -- for mouse movement.
  , shininess :: GLfloat
  }

renderingLoop :: GLFW.Window -> Mesh.Object -> IO ()
renderingLoop window mesh = do
  GLFW.setCursorInputMode window GLFW.CursorInputMode'Disabled
  curPos <- GLFW.getCursorPos window
  camera <- newIORef $ Main.Camera
    { pos = vec3 25 25 50
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
      display camera mesh
      GLFW.swapBuffers window
      GLFW.pollEvents

      c <- readIORef camera
      let zeroVector = vec3 0 0 0
      let targetDistance = (target c) - (pos c)
          frontVector = V.normalize targetDistance
          leftVector = V.cross (up c) frontVector

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

instance NearZero CFloat where
  nearZero 0 = True
  nearZero _ = False

calcMatrix :: Camera -> Mat44 GLfloat -> [Mat44 GLfloat]
calcMatrix c modelMatrix =
  [mvp, mv, n]
  where
    viewMatrix = Main.lookAt (pos c) (target c) (up c)
    projMatrix = (V.perspective 0.1 100 (pi / 4) (4 / 3)) :: Mat44 GLfloat
    mv = V.multmm viewMatrix modelMatrix
    mvp = V.multmm projMatrix mv
    n = V.transpose (fromJust (V.invert mv))

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

display :: IORef Camera -> Mesh.Object -> IO ()
display camera mesh = do
  glClearColor 0.1 0.4 0.2 1
  glClear $ gl_COLOR_BUFFER_BIT .|. gl_DEPTH_BUFFER_BIT

  c <- readIORef camera
  let [mvp, mv, n] = calcMatrix c V.identity
      s = shininess c

  Mesh.draw mesh mvp mv n s

  -- translation test.
  let v2 = 20 :. 0 :. 0 :. () :: Vec3 CFloat
      m2 = V.translate v2 (V.identity :: Mat44 CFloat)
  let [mvp', mv', n'] = calcMatrix c m2
  Mesh.draw mesh mvp' mv' n' s

  glFlush
