{-# LANGUAGE PackageImports #-}
{-# LANGUAGE RecordWildCards #-}
module Mesh
  ( Object(..)
  , create
  , draw
  , destroy
  )
 where

import Graphics.Rendering.OpenGL
import Graphics.Rendering.OpenGL.Raw
import Foreign.Storable
import Foreign.Marshal.Array
import Foreign.Ptr
import Foreign.Marshal.Utils
import qualified Shader as Shader

-- | The object that has any rendering status.
data Object = Object
  { vao :: VertexArrayObject
  , vertexBuffer :: BufferObject
  , indexBuffer :: BufferObject
  , indicesOffset :: ArrayIndex
  , numArrayIndices :: NumArrayIndices
  , program :: Program
  , mvpUniformLocation :: UniformLocation -- | Model-View-Projection matrix.
  , mUniformLocation :: UniformLocation -- | Model matrix(for lighting).
  }

-- | Get the pointer of buffer from the offset.
bufferOffset :: (Integral a) => a -> Ptr b
bufferOffset = plusPtr nullPtr . fromIntegral

-- | Get the byte size of a array.
arrayByteSize :: (Storable a) => [a] -> Int
arrayByteSize xs = (sizeOf $ head xs) * (length xs)

-- | Create the OpenGL buffer that has a target type and is initialized a data array.
createBuffer :: (Storable a) => BufferTarget -> [a] -> IO BufferObject
createBuffer target xs = do
  buffer <- genObjectName
  bindBuffer target $= Just buffer
  withArray xs $ \ptr -> do
    bufferData target $= (fromIntegral $ arrayByteSize xs, ptr, StaticDraw)
  bindBuffer target $= Nothing
  return buffer

-- | Create the object.
create :: (Storable a, Storable b) => [a] -> [b] -> IO Object
create vertices indices = do
  vao <- genObjectName
  vb <- createBuffer ArrayBuffer vertices
  ib <- createBuffer ElementArrayBuffer indices
  bindVertexArrayObject $= Just vao
  bindBuffer ArrayBuffer $= Just vb
  bindBuffer ElementArrayBuffer $= Just ib
  
  let numPositionElements = 3
      numNormalElements = 3
      numColorElements = 4
      vPosition = AttribLocation 0
      vNormal = AttribLocation 1
      vColor = AttribLocation 2
      offsetPosition = 0
      offsetNormal = offsetPosition + numPositionElements
      offsetColor = offsetNormal + numColorElements
      sizeElement = sizeOf $ head vertices
      sizeVertex = fromIntegral $ sizeElement * (numPositionElements + numNormalElements + numColorElements)

  vertexAttribPointer vPosition $=
    ( ToFloat
    , VertexArrayDescriptor (fromIntegral numPositionElements) Float sizeVertex (bufferOffset (offsetPosition * sizeElement))
    )
  vertexAttribArray vPosition $= Enabled

  vertexAttribPointer vNormal $=
    ( ToFloat
    , VertexArrayDescriptor (fromIntegral numNormalElements) Float sizeVertex (bufferOffset (offsetNormal * sizeElement))
    )
  vertexAttribArray vNormal $= Enabled

  vertexAttribPointer vColor $=
    ( ToFloat
    , VertexArrayDescriptor (fromIntegral numColorElements) Float sizeVertex (bufferOffset (offsetColor * sizeElement))
    )
  vertexAttribArray vColor $= Enabled

  bindVertexArrayObject $= Nothing

  program <- Shader.load 
    [ Shader.Info VertexShader (Shader.FileSource "shaders/default.vert")
    , Shader.Info FragmentShader (Shader.FileSource "shaders/default.frag")
    ]
  mvpLocation <- get $ uniformLocation program "MVP"
  mLocation <- get $ uniformLocation program "M"
  return Object
    { vao = vao
    , vertexBuffer = vb
    , indexBuffer = ib
    , indicesOffset = 0
    , numArrayIndices = fromIntegral $ length indices
    , program = program
    , mvpUniformLocation = mvpLocation
    , mUniformLocation = mLocation
    }

-- | Draw the object.
draw :: (Storable a) => Object -> a -> a -> IO ()
draw obj mvp m = do
  currentProgram $= Just (program obj)
  bindVertexArrayObject $= Just (vao obj)

  let (UniformLocation mvpLocation) = mvpUniformLocation obj
  let (UniformLocation mLocation) = mUniformLocation obj
  with mvp $ glUniformMatrix4fv (fromIntegral mvpLocation) 1 (fromBool True) . castPtr
  with m $ glUniformMatrix4fv (fromIntegral mLocation) 1 (fromBool True) . castPtr
  drawElements Triangles (numArrayIndices obj) UnsignedShort (bufferOffset $ indicesOffset obj) 

  bindVertexArrayObject $= Nothing
  currentProgram $= Nothing
  return ()

-- | Destroy the object.
destroy :: Object -> IO ()
destroy obj = do
  currentProgram $= Nothing
  shaders <- get $ attachedShaders $ program obj
  mapM_ releaseShader shaders
  deleteObjectName $ program obj
  deleteObjectName $ indexBuffer obj
  deleteObjectName $ vertexBuffer obj
  deleteObjectName $ vao obj
  where
    releaseShader shader = do
      detachShader (program obj) shader
      deleteObjectName shader
