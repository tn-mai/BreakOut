{-# LANGUAGE PackageImports #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
module Mesh
  ( Environment(..)
  , createEnvironment
  , Object(..)
  , create
  , draw
  , destroy
  , NearZero
  , vec4
  , createBuffer
  , bufferOffset
  )
 where

import Graphics.Rendering.OpenGL as GL
import Graphics.Rendering.OpenGL.Raw
import Graphics.Rendering.OpenGL.GL.ByteString
import Graphics.Rendering.OpenGL.Raw.ARB.UniformBufferObject
import Foreign.Storable
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.Ptr
import Foreign.C.Types
import Foreign.Marshal.Utils
import qualified Shader as Shader
import Data.Vec as Vec
import Data.Maybe
import LightSource

vec4 :: forall a a1 a2 a3. a -> a1 -> a2 -> a3 -> a :. (a1 :. (a2 :. (a3 :. ())))
vec4 x y z w = x :. y :. z :. w :. ()

data Environment = Environment
  { materialBufferId :: CUInt
  }

createEnvironment :: IO Environment
createEnvironment = do
  id <- alloca $ \ptr -> do
    let bufferId = 6
    glGenBuffers 1 ptr
    buffer <- peek ptr
    glBindBuffer gl_UNIFORM_BUFFER buffer
    glBindBufferBase gl_UNIFORM_BUFFER bufferId buffer
    glBufferData gl_UNIFORM_BUFFER 128 nullPtr gl_DYNAMIC_DRAW
    glBindBuffer gl_UNIFORM_BUFFER 0
    return buffer
  return Environment
    { materialBufferId = fromIntegral $ toInteger id
    }

-- | The object that has any rendering status.
data Object = Object
  { vao :: VertexArrayObject
  , vertexBuffer :: BufferObject
  , indexBuffer :: BufferObject
  , indicesOffset :: ArrayIndex
  , numArrayIndices :: NumArrayIndices
  , program :: Shader.Program
  , mvpUniformLocation :: Shader.UniformLocation -- | Model-View-Projection matrix.
  , mvUniformLocation :: Shader.UniformLocation -- | Model-View matrix(for lighting).
  , normalUniformLocation :: Shader.UniformLocation -- | Normal matrix(for lighting).
  , material :: Material
  }

defaultMaterial :: Material
defaultMaterial = Material
  { baseColor = vec4 0.8 0.8 0.9 1
  , metallic = 0.5
  , roughness = 0.1
  }

-- | Get the pointer of buffer from the offset.
bufferOffset :: (Integral a) => a -> Ptr b
bufferOffset = plusPtr nullPtr . fromIntegral

-- | Get the byte size of a array.
arrayByteSize :: (Storable a) => [a] -> Int
arrayByteSize xs = (sizeOf $ Prelude.head xs) * (Prelude.length xs)

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
      offsetColor = offsetNormal + numNormalElements
      sizeElement = sizeOf $ Prelude.head vertices
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
    [ Shader.Info gl_VERTEX_SHADER (Shader.FileSource "shaders/default.vert")
    , Shader.Info gl_FRAGMENT_SHADER (Shader.FileSource "shaders/default.frag")
    ]
  mvpLocation <- Shader.uniformLocation program "MVP"
  mvLocation <- Shader.uniformLocation program "MV"
  normalLocation <- Shader.uniformLocation program "N"
  return Object
    { vao = vao
    , vertexBuffer = vb
    , indexBuffer = ib
    , indicesOffset = 0
    , numArrayIndices = fromIntegral $ Prelude.length indices
    , program = program
    , mvpUniformLocation = mvpLocation
    , mvUniformLocation = mvLocation
    , normalUniformLocation = normalLocation
    , material = defaultMaterial
    }

instance NearZero CFloat where
  nearZero 0 = True
  nearZero _ = False

-- | Draw the object.
draw :: Environment -> Object -> Mat44 GLfloat -> Mat44 GLfloat -> Mat44 GLfloat -> IO ()
draw env obj modelMatrix viewMatrix projectionMatrix = do
  Shader.useProgram $ Just (program obj)
  bindVertexArrayObject $= Just (vao obj)

  let mvMatrix = (Vec.multmm viewMatrix modelMatrix) :: Mat44 GLfloat
      mvpMatrix = (Vec.multmm projectionMatrix mvMatrix) :: Mat44 GLfloat
      normalMatrix = Vec.transpose (fromJust (Vec.invert mvMatrix))

  let mvpLocation = mvpUniformLocation obj
      mvLocation = mvUniformLocation obj
      normalLocation = normalUniformLocation obj

  with mvpMatrix $ glUniformMatrix4fv (fromIntegral mvpLocation) 1 (fromBool True) . castPtr
  with mvMatrix $ glUniformMatrix4fv (fromIntegral mvLocation) 1 (fromBool True) . castPtr
  with normalMatrix $ glUniformMatrix4fv (fromIntegral normalLocation) 1 (fromBool True) . castPtr

  let bufferId = 6
  glBindBuffer gl_UNIFORM_BUFFER $ materialBufferId env
  with (material obj) $ \mat -> do
    glBufferSubData gl_UNIFORM_BUFFER 0 (fromIntegral (sizeOf (material obj))) mat
  glBindBuffer gl_UNIFORM_BUFFER 0
  glBindBufferBase gl_UNIFORM_BUFFER bufferId $ materialBufferId env
  idx <- withGLstring "Material" $ glGetUniformBlockIndex (Shader.programId $ program obj)
  glUniformBlockBinding (Shader.programId (program obj)) idx bufferId

  drawElements Triangles (numArrayIndices obj) UnsignedShort (bufferOffset $ indicesOffset obj) 

  bindVertexArrayObject $= Nothing
  Shader.useProgram Nothing
  return ()

-- | Destroy the object.
destroy :: Object -> IO ()
destroy obj = do
  Shader.unload $ program obj
  deleteObjectName $ indexBuffer obj
  deleteObjectName $ vertexBuffer obj
  deleteObjectName $ vao obj
