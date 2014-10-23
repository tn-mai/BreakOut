{-# LANGUAGE PackageImports #-}
module App
  ( create
  , destroy
  )
where

import qualified "GLFW-b" Graphics.UI.GLFW as GLFW
import Graphics.Rendering.OpenGL.Raw as GLRaw
import System.IO

-- | Create the OpenGL window.
create :: String -> Int -> Int -> IO (Maybe GLFW.Window)
create windowTitle width height = do
  GLFW.setErrorCallback $ Just (\e d -> do hPutStrLn stderr $ (show e) ++ ": " ++ d)
  ok <- GLFW.init
  if (not ok)
  then do
    _ <- fail "Failed to initialize GLFW"
    return Nothing
  else do
    mapM_ GLFW.windowHint
      [ GLFW.WindowHint'Samples 4
      , GLFW.WindowHint'ContextVersionMajor 3
      , GLFW.WindowHint'ContextVersionMinor 3
      , GLFW.WindowHint'OpenGLProfile GLFW.OpenGLProfile'Core
      ]
    r <- GLFW.createWindow width height windowTitle Nothing Nothing
    case r of
      Nothing -> do
        _ <- fail "Failed to create window"
        GLFW.terminate
        return Nothing
      Just window -> do
        GLFW.makeContextCurrent $ Just window
        GLFW.swapInterval 1
        GLFW.setStickyKeysInputMode window GLFW.StickyKeysInputMode'Enabled

        GLRaw.glEnable gl_CULL_FACE
        GLRaw.glCullFace gl_BACK
        GLRaw.glFrontFace gl_CW

        return $ Just window

-- | Destroy the OpenGL window.
destroy :: GLFW.Window -> IO ()
destroy window = do
  GLFW.destroyWindow window
  GLFW.terminate
  return ()
