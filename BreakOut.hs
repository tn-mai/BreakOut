{-# LANGUAGE PackageImports #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
module Main where

import qualified App as App
import qualified Mesh as Mesh

import qualified "GLFW-b" Graphics.UI.GLFW as GLFW
import Graphics.Rendering.OpenGL
import Data.Maybe (isNothing, fromJust)
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
        7---6
  left /|  /|
      / 4-/-5
     3---2 / right
     |/  |/
     0---1
  front
       bottom
  --}
  let vertices0 = 
        -- front
        [ (-0.2), (-0.2), ( 0.1), ( 0.0), ( 0.0), ( 1.0), 1.0, 1.0, 1.0, 1.0
        , ( 0.2), (-0.2), ( 0.1), ( 0.0), ( 0.0), ( 1.0), 1.0, 1.0, 1.0, 1.0
        , ( 0.2), ( 0.2), ( 0.1), ( 0.0), ( 0.0), ( 1.0), 1.0, 1.0, 1.0, 1.0
        , (-0.2), ( 0.2), ( 0.1), ( 0.0), ( 0.0), ( 1.0), 1.0, 1.0, 1.0, 1.0

        -- back
        , (-0.2), (-0.2), (-0.1), ( 0.0), ( 0.0), (-1.0), 1.0, 1.0, 1.0, 1.0
        , ( 0.2), (-0.2), (-0.1), ( 0.0), ( 0.0), (-1.0), 1.0, 1.0, 1.0, 1.0
        , ( 0.2), ( 0.2), (-0.1), ( 0.0), ( 0.0), (-1.0), 1.0, 1.0, 1.0, 1.0
        , (-0.2), ( 0.2), (-0.1), ( 0.0), ( 0.0), (-1.0), 1.0, 1.0, 1.0, 1.0

        -- right
        , ( 0.2), (-0.2), ( 0.1), ( 1.0), ( 0.0), ( 0.0), 1.0, 1.0, 1.0, 1.0
        , ( 0.2), (-0.2), (-0.1), ( 1.0), ( 0.0), ( 0.0), 1.0, 1.0, 1.0, 1.0
        , ( 0.2), ( 0.2), (-0.1), ( 1.0), ( 0.0), ( 0.0), 1.0, 1.0, 1.0, 1.0
        , ( 0.2), ( 0.2), ( 0.1), ( 1.0), ( 0.0), ( 0.0), 1.0, 1.0, 1.0, 1.0

        -- left
        , (-0.2), (-0.2), (-0.1), (-1.0), ( 0.0), ( 0.0), 1.0, 1.0, 1.0, 1.0
        , (-0.2), (-0.2), ( 0.1), (-1.0), ( 0.0), ( 0.0), 1.0, 1.0, 1.0, 1.0
        , (-0.2), ( 0.2), ( 0.1), (-1.0), ( 0.0), ( 0.0), 1.0, 1.0, 1.0, 1.0
        , (-0.2), ( 0.2), (-0.1), (-1.0), ( 0.0), ( 0.0), 1.0, 1.0, 1.0, 1.0

        -- top
        , ( 0.2), ( 0.2), ( 0.1), ( 0.0), ( 1.0), ( 0.0), 1.0, 1.0, 1.0, 1.0
        , ( 0.2), (-0.2), ( 0.1), ( 0.0), ( 1.0), ( 0.0), 1.0, 1.0, 1.0, 1.0
        , ( 0.2), (-0.2), (-0.1), ( 0.0), ( 1.0), ( 0.0), 1.0, 1.0, 1.0, 1.0
        , ( 0.2), ( 0.2), (-0.1), ( 0.0), ( 1.0), ( 0.0), 1.0, 1.0, 1.0, 1.0

        -- bottom
        , (-0.2), (-0.2), (-0.1), ( 0.0), (-1.0), ( 0.0), 1.0, 1.0, 1.0, 1.0
        , ( 0.2), (-0.2), (-0.1), ( 0.0), (-1.0), ( 0.0), 1.0, 1.0, 1.0, 1.0
        , ( 0.2), (-0.2), ( 0.1), ( 0.0), (-1.0), ( 0.0), 1.0, 1.0, 1.0, 1.0
        , (-0.2), (-0.2), ( 0.1), ( 0.0), (-1.0), ( 0.0), 1.0, 1.0, 1.0, 1.0
        ] :: [GLfloat]
  let indices0 =
        [ 2, 1, 0, 0, 3, 2
        , 4, 5, 6, 6, 7, 4
        , 6, 5, 1, 1, 2, 6
        , 3, 0, 4, 4, 7, 3
        , 6, 2, 3, 3, 7, 6
        , 1, 5, 4, 4, 0, 1
        ] :: [GLushort]
  mesh0 <- Mesh.create vertices0 indices0
  renderingLoop (fromJust r) mesh0
  Mesh.destroy mesh0
  App.destroy (fromJust r)
  
renderingLoop :: GLFW.Window -> Mesh.Object -> IO ()
renderingLoop window mesh = do
  loop
  where
    loop = (GLFW.windowShouldClose window) >>= (flip unless) go
    go = do
      display mesh
      GLFW.swapBuffers window
      GLFW.pollEvents
      threadDelay 10000
      loop

vec3 :: forall a a1 a2. a -> a1 -> a2 -> a :. (a1 :. (a2 :. ()))
vec3 x y z = x :. y :. z :. ()

mvpMatrix :: Mat44 GLfloat -> Mat44 GLfloat
mvpMatrix model = V.multmm (V.multmm projection view) model
  where
    projection = V.perspective 0.1 100 (pi / 4) (4 / 3)
    view = Main.lookAt (vec3 4 3 3) (vec3 0 0 0) (vec3 0 1 0)

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

display :: Mesh.Object -> IO ()
display mesh = do
  clearColor $= Color4 0.1 0.4 0.2 1
  clear [ColorBuffer, DepthBuffer]

  Mesh.draw mesh (mvpMatrix V.identity)

  -- translation test.
  let v2 = 2 :. 0 :. 0 :. () :: Vec3 CFloat
      m2 = V.translate v2 (V.identity :: Mat44 CFloat)
  Mesh.draw mesh (mvpMatrix m2)

  flush
