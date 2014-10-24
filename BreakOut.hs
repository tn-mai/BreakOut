{-# LANGUAGE PackageImports #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
module Main where

import qualified App as App
import qualified Mesh as Mesh

import qualified "GLFW-b" Graphics.UI.GLFW as GLFW
import Graphics.Rendering.OpenGL
import Data.Maybe (isNothing, fromJust)
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

data Position = Position GLfloat GLfloat GLfloat
  
renderingLoop :: GLFW.Window -> Mesh.Object -> IO ()
renderingLoop window mesh = do
  cameraTarget <- newIORef $ Main.Position 40 30 30
  loop cameraTarget
  where
    loop ct = (GLFW.windowShouldClose window) >>= (flip unless) (go ct)
    go ct = do
      display ct mesh
      GLFW.swapBuffers window
      GLFW.pollEvents

      keyStateW <- GLFW.getKey window GLFW.Key'W
      when (keyStateW == GLFW.KeyState'Pressed) $ do
        (Main.Position x y z) <- readIORef ct
        writeIORef ct $ Main.Position x y (z + 0.2)

      keyStateA <- GLFW.getKey window GLFW.Key'A
      when (keyStateA == GLFW.KeyState'Pressed) $ do
        (Main.Position x y z) <- readIORef ct
        writeIORef ct $ Main.Position (x - 0.2) y z

      keyStateS <- GLFW.getKey window GLFW.Key'S
      when (keyStateS == GLFW.KeyState'Pressed) $ do
        (Main.Position x y z) <- readIORef ct
        writeIORef ct $ Main.Position x y (z - 0.2)

      keyStateD <- GLFW.getKey window GLFW.Key'D
      when (keyStateD == GLFW.KeyState'Pressed) $ do
        (Main.Position x y z) <- readIORef ct
        writeIORef ct $ Main.Position (x + 0.2) y z

      isExit <- GLFW.getKey window GLFW.Key'Escape
      when (isExit /= GLFW.KeyState'Pressed) $ do
        threadDelay 10000
        loop ct

vec3 :: forall a a1 a2. a -> a1 -> a2 -> a :. (a1 :. (a2 :. ()))
vec3 x y z = x :. y :. z :. ()

instance NearZero CFloat where
  nearZero 0 = True
  nearZero _ = False

calcMatrix :: Vec3 GLfloat -> Mat44 GLfloat -> [Mat44 GLfloat]
calcMatrix ct modelMatrix =
  [mvp, mv, n]
  where
    viewMatrix = Main.lookAt ct (vec3 0 0 0) (vec3 0 1 0)
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

display :: IORef Main.Position -> Mesh.Object -> IO ()
display cameraTarget mesh = do
  clearColor $= Color4 0.1 0.4 0.2 1
  clear [ColorBuffer, DepthBuffer]

  (Main.Position x y z) <- readIORef cameraTarget
  let [mvp, mv, n] = calcMatrix (vec3 x y z) V.identity

  Mesh.draw mesh mvp mv n

  -- translation test.
  let v2 = 20 :. 0 :. 0 :. () :: Vec3 CFloat
      m2 = V.translate v2 (V.identity :: Mat44 CFloat)
  let [mvp', mv', n'] = calcMatrix (vec3 x y z) m2
  Mesh.draw mesh mvp' mv' n'

  flush
