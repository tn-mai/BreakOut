{-# LANGUAGE PackageImports #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
module Mesh
  ( Object(..)
  , create
  , draw
  , destroy
  , NearZero
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
  , shininessUniformLocation :: Shader.UniformLocation
  , lightUniformLocation :: Shader.UniformLocation
  , materialUniformLocation :: Shader.UniformLocation
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
      offsetColor = offsetNormal + numColorElements
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
  shineLocation <- Shader.uniformLocation program "shininess"
  lightLocation <- Shader.uniformLocation program "LightSource"
  materialLocation <- Shader.uniformLocation program "Material"
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
    , shininessUniformLocation = shineLocation
    , lightUniformLocation = lightLocation
    , materialUniformLocation = materialLocation
    }

lightSource :: LightSource
lightSource = LightSource
  { diffuse = vec4 1 1 1 1
  , specular = vec4 1 1 1 1
  , position = vec4 50 50 100 1
  , attenuation = 1
  }

instance NearZero CFloat where
  nearZero 0 = True
  nearZero _ = False

-- | Draw the object.
draw :: Object -> Mat44 GLfloat -> Mat44 GLfloat -> Mat44 GLfloat -> IO ()
draw obj modelMatrix viewMatrix projectionMatrix = do
  Shader.useProgram $ Just (program obj)
  bindVertexArrayObject $= Just (vao obj)

  let mvMatrix = (Vec.multmm viewMatrix modelMatrix) :: Mat44 GLfloat
      mvpMatrix = (Vec.multmm projectionMatrix mvMatrix) :: Mat44 GLfloat
      normalMatrix = Vec.transpose (fromJust (Vec.invert mvMatrix))

  let mvpLocation = mvpUniformLocation obj
      mvLocation = mvUniformLocation obj
      normalLocation = normalUniformLocation obj
      shineLocation = shininessUniformLocation obj

  with mvpMatrix $ glUniformMatrix4fv (fromIntegral mvpLocation) 1 (fromBool True) . castPtr
  with mvMatrix $ glUniformMatrix4fv (fromIntegral mvLocation) 1 (fromBool True) . castPtr
  with normalMatrix $ glUniformMatrix4fv (fromIntegral normalLocation) 1 (fromBool True) . castPtr
  glUniform1f (fromIntegral shineLocation) 64

  let newPos = Vec.multmv viewMatrix (LightSource.position lightSource)
      newLS = lightSource { LightSource.position = newPos }

  alloca $ \ptr -> do
    let bufferId = 7
    glGenBuffers 1 ptr
    buffer <- peek ptr
    glBindBuffer gl_UNIFORM_BUFFER buffer
    with newLS $ \ls -> do
      glBufferData gl_UNIFORM_BUFFER 52 ls gl_STATIC_DRAW 
    glBindBuffer gl_UNIFORM_BUFFER 0
    glBindBufferBase gl_UNIFORM_BUFFER bufferId buffer
    idx <- withGLstring "LightSource" $ glGetUniformBlockIndex (Shader.programId $ program obj)
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
