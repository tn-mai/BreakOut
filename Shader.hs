module Shader
  ( Source(..)
  , Info(..)
  , load
  , unload
  )
where

import Control.Exception
import Control.Monad
import Graphics.Rendering.OpenGL as GL
import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Text.Encoding as E

data Source = ByteSource BS.ByteString | StringSource String | FileSource FilePath deriving (Eq, Ord, Show)
data Info = Info ShaderType Source deriving (Eq, Ord, Show)

{-- | Create the shader program object, and load, compile and attach the shaders.
--}
load :: [Info]
     -> IO Program
load infoList =
  bracketOnError
    createProgram
    deleteObjectName
    (\p -> do
      loadCompileAttach p infoList
      checked linkProgram linkStatus programInfoLog "link" p
      return p
    )
  where
    loadCompileAttach :: Program -> [Info] -> IO ()
    loadCompileAttach _ [] = return ()
    loadCompileAttach program (Info shType shSource : xs) =
      bracketOnError
        (createShader shType)
        deleteObjectName
        (\shader -> do
          bin <- toBinary shSource
          shaderSourceBS shader $= bin
          checked compileShader compileStatus shaderInfoLog "compile" shader
          attachShader program shader
          loadCompileAttach program xs)

{-- | Detach and release all of shaders and delete the shader program object.
--}
unload :: Program -> IO ()
unload program = do
  currentProgram $= Nothing
  shaders <- get $ attachedShaders program
  mapM_ releaseShader shaders
  deleteObjectName program
  where
    releaseShader shader = do
      detachShader program shader
      deleteObjectName shader

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
