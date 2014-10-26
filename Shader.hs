module Shader
  ( Program(..)
  , Source(..)
  , Info(..)
  , UniformLocation
  , load
  , unload
  , uniformLocation
  , useProgram
  )
where

import Control.Exception
import Control.Monad
import Foreign.Marshal.Alloc
import Foreign.Marshal.Utils
import Foreign.Marshal.Array
import Foreign.Ptr
import Foreign.Storable
--import Graphics.Rendering.OpenGL.GL.GLboolean
--import Graphics.Rendering.OpenGL.GL.PeekPoke
import Graphics.Rendering.OpenGL.GL.ByteString
import Graphics.Rendering.OpenGL.GL.StateVar
import Graphics.Rendering.OpenGL.Raw
import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Text.Encoding as E

data Program = Program { programId :: GLuint }

data Source = ByteSource BS.ByteString | StringSource String | FileSource FilePath deriving (Eq, Ord, Show)
data Info = Info GLenum Source deriving (Eq, Ord, Show)

type UniformLocation = GLint

{-- | Create the shader program object, and load, compile and attach the shaders.
--}
load :: [Info]
     -> IO Program
load infoList = do
  bracketOnError
    glCreateProgram
    glDeleteProgram
    (\progId -> do
      loadCompileAttach progId infoList
      checked glLinkProgram linkStatus programInfoLog "link" progId
      return $ Program progId
    )
  where
    loadCompileAttach :: GLuint -> [Info] -> IO ()
    loadCompileAttach _ [] = return ()
    loadCompileAttach progId (Info shType shSource : xs) = do
      bracketOnError
        (glCreateShader shType)
        glDeleteShader
        (\shaderId -> do
          bin <- toBinary shSource
          setShaderSource shaderId bin
          checked glCompileShader compileStatus shaderInfoLog "compile" shaderId
          glAttachShader progId shaderId
          loadCompileAttach progId xs)

    setShaderSource :: GLuint -> BS.ByteString -> IO ()
    setShaderSource shaderId bin =
      withByteString bin $ \ptr len ->
        with ptr $ \ptrBuf ->
          with len $ \lenBuf ->
            glShaderSource shaderId 1 ptrBuf lenBuf

    linkStatus :: GLuint -> GettableStateVar Bool
    linkStatus = programVar unmarshalGLboolean gl_LINK_STATUS

    programInfoLog :: GLuint -> GettableStateVar String
    programInfoLog =
      makeGettableStateVar . fmap unpackUtf8 . stringQuery programInfoLogLength glGetProgramInfoLog

    programInfoLogLength :: GLuint -> GettableStateVar GLsizei
    programInfoLogLength = programVar fromIntegral gl_INFO_LOG_LENGTH

    compileStatus :: GLuint -> GettableStateVar Bool
    compileStatus = shaderVar unmarshalGLboolean gl_COMPILE_STATUS

    shaderInfoLog :: GLuint -> GettableStateVar String
    shaderInfoLog =
      makeGettableStateVar . fmap unpackUtf8 . stringQuery shaderInfoLogLength glGetShaderInfoLog

    shaderInfoLogLength :: GLuint -> GettableStateVar GLsizei
    shaderInfoLogLength = shaderVar fromIntegral gl_INFO_LOG_LENGTH

{-- | Detach and release all of shaders and delete the shader program object.
--}
unload :: Program -> IO ()
unload (Program progId) = do
  glUseProgram 0
  shaders <- getAttachedShaders progId
  mapM_ releaseShader shaders
  glDeleteProgram progId
  where
    releaseShader shaderId = do
      glDetachShader progId shaderId
      glDeleteShader shaderId

    getAttachedShaders :: GLuint -> IO [GLuint]
    getAttachedShaders progId' = do
      numShaders <- get $ programVar fromIntegral gl_ATTACHED_SHADERS progId'
      ids <- allocaArray (fromIntegral numShaders) $ \buf -> do
        glGetAttachedShaders progId' numShaders nullPtr buf
        peekArray (fromIntegral numShaders) buf
      return ids

unmarshalGLboolean :: (Num a, Eq a) => a -> Bool
unmarshalGLboolean = (/= 0)

peek1 :: Storable a => (a -> b) -> Ptr a -> IO b
peek1 f ptr = do
  x <- peekElemOff ptr 0
  return $ f x

shaderVar :: (GLint -> a) -> GLenum -> GLuint -> GettableStateVar a
shaderVar f p shaderId =
  makeGettableStateVar $ alloca $ \buf -> do
    glGetShaderiv shaderId p buf
    peek1 f buf

programVar :: (GLint -> a) -> GLenum -> GLuint -> GettableStateVar a
programVar f p progId =
  makeGettableStateVar $ alloca $ \buf -> do
    glGetProgramiv progId p buf
    peek1 f buf

toBinary :: Source
         -> IO BS.ByteString
toBinary (ByteSource bs) = return bs
toBinary (StringSource ss) = return . E.encodeUtf8 $ T.pack ss
toBinary (FileSource fs) = BS.readFile fs

checked :: (t -> IO ())
        -> (t -> GettableStateVar Bool)
        -> (t -> GettableStateVar String)
        -> String
        -> t
        -> IO ()
checked action getStatus getLog errorMessage object = do
  action object
  r <- get $ getStatus object
  unless r $ do
    actionLog <- get $ getLog object
    fail $ errorMessage ++ " log: " ++ actionLog

uniformLocation :: Program -> String -> IO UniformLocation
uniformLocation (Program progId) name = withGLstring name $ glGetUniformLocation progId

useProgram :: Maybe Program -> IO ()
useProgram (Just (Program progId)) = glUseProgram progId
useProgram _ = glUseProgram 0
