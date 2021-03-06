{-# LANGUAGE PackageImports #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
module Main where

import qualified App as App
import qualified Mesh as Mesh
import Mesh (vec4)
import qualified Shader as Shader
import qualified BarMesh as BarMesh
import qualified LightSource as LS
import qualified TitleData
import qualified Collision

import qualified "GLFW-b" Graphics.UI.GLFW as GLFW
import Graphics.Rendering.OpenGL hiding (TextureObject, Line)
import Graphics.Rendering.OpenGL.Raw as GLRaw
import Graphics.Rendering.OpenGL.GL.ByteString
import Data.Maybe (isNothing, fromJust)
import Data.Char (ord)
import Data.List (sortBy)
import Data.Ord (comparing)
import Data.Bits
import Data.Array.Storable
import Data.Time.Clock
import Data.Vec as V
import Text.Printf
import Control.Monad
import Control.Concurrent (threadDelay)
import System.Environment
import System.Exit
import System.IO
import Foreign.C.Types
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.Storable
import "pngload" Codec.Image.PNG

-- | Make the 2 coordinate vector.
vec2 :: forall a a1. a -> a1 -> a :. (a1 :. ())
vec2 x y = x :. y :. ()

-- | Make the 3 coordinate vector.
vec3 :: forall a a1 a2. a -> a1 -> a2 -> a :. (a1 :. (a2 :. ()))
vec3 x y z = x :. y :. z :. ()

type Offset2 = V.Vec2 GLfloat
type Scale2 = V.Vec2 GLfloat

-- | Actor is the mesh object controller.
data Actor = Actor
  { object :: Mesh.Object
  , matrix :: V.Mat44 GLfloat
  , position :: Vec3 GLfloat
  }

-- | the light source list.
lightSource :: [LS.LightSource]
lightSource =
  [ LS.LightSource
      { LS.diffuse =  vec4 1000000 1000000 1000000 1
      , LS.specular = vec4  200000  500000  700000 1
      , LS.position = vec4 (-300) 300 (-300) 1
      }
  , LS.LightSource
      { LS.diffuse =  vec4 0 0 1000000 1
      , LS.specular = vec4  20000  10000  10000 1
      , LS.position = vec4 300 (-300) 300 1
      }
  , LS.LightSource
      { LS.diffuse =  vec4 1000000 1000000 1000000 1
      , LS.specular = vec4  500000  700000  900000 1
      , LS.position = vec4 (-300) 300 (-300) 0
      }
  , LS.LightSource
      { LS.diffuse =  vec4 1000000 1000000 1000000 1
      , LS.specular = vec4  500000  700000  900000 1
      , LS.position = vec4 (-300) 300 (-300) 0
      }
  ]

-- | Transform the light source position in the view coordinates.
toViewSpace :: Mat44 GLfloat -> LS.LightSource -> LS.LightSource
toViewSpace m ls = ls { LS.position = V.multmv m (LS.position ls) }

-- | the material list for any mesh object.
materials :: [LS.Material]
materials =
  [ LS.Material
    { LS.baseColor = Mesh.vec4 0.8 0.5 0.1 1
    , LS.metallic = 0.5
    , LS.roughness = 0.1
    }
  , LS.Material
    { LS.baseColor = Mesh.vec4 0.6 0.5 0.4 1
    , LS.metallic = 0.5
    , LS.roughness = 0.1
    }
  , LS.Material
    { LS.baseColor = Mesh.vec4 0.8 0.2 0.3 1
    , LS.metallic = 0.3
    , LS.roughness = 0.6
    }
  , LS.Material
    { LS.baseColor = Mesh.vec4 0.3 0.9 0.2 1
    , LS.metallic = 0.1
    , LS.roughness = 0.6
    }
  , LS.Material
    { LS.baseColor = Mesh.vec4 0.2 0.6 0.5 1
    , LS.metallic = 0.7
    , LS.roughness = 0.1
    }
  ]

-- | the material list for the title mesh object.
titleMaterials :: [LS.Material]
titleMaterials =
  [ LS.Material
    { LS.baseColor = Mesh.vec4 0.7 0.2 0.1 1
    , LS.metallic = 0.1
    , LS.roughness = 0.3
    }
  , LS.Material
    { LS.baseColor = Mesh.vec4 0.01 0.0 0.02 0.5
    , LS.metallic = 0.0
    , LS.roughness = 1.0
    }
  ]

blockScaleX :: GLfloat
blockScaleX = 2.5
blockSizeX :: GLfloat
blockSizeX = BarMesh.cubeSize * blockScaleX

blockScaleY :: GLfloat
blockScaleY = 1.25
blockSizeY :: GLfloat
blockSizeY = BarMesh.cubeSize * blockScaleY

blockScaleZ :: GLfloat
blockScaleZ = 1.25
blockSizeZ :: GLfloat
blockSizeZ = BarMesh.cubeSize * blockScaleZ

-- | Make the initial actors of the stage.
initialActors :: Mesh.Object -> [Actor]
initialActors cubeMesh =
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
        (V.scale (vec4 blockScaleX blockScaleY blockScaleZ 1) identityMatrix)
        $ [(x, y, (-30)) | x <- [25, 75 .. 200], y <- [200, 225 .. 300]]
      ballActor = makeActors
        cubeMesh { Mesh.material = materials !! 4 }
        (V.scale (vec4 0.5 0.5 0.5 1) identityMatrix)
        $ [(125, 100, -30)]
  in paddleActor ++ ballActor ++ floorActors ++ wallActors ++ sideWallActors ++ blockActors


-- | The entry point.
main :: IO ()
main = do
  progName <- getProgName
  r <- App.create progName 800 600
  when (isNothing r) exitFailure
  cubeMesh <- Mesh.create BarMesh.chamferedCubeVertices BarMesh.chamferedCubeIndices
  renderingLoop (fromJust r) cubeMesh
  Mesh.destroy cubeMesh
  App.destroy (fromJust r)

-- | Make the actors from the initializer list.
makeActors :: Mesh.Object -> Mat44 CFloat -> [(CFloat, CFloat, CFloat)] -> [Actor]
makeActors mesh mat posList =
  Prelude.map
    (\(x, y, z) -> Actor mesh mat (vec3 x y z))
    posList

rgb8 :: GLint
rgb8 = 0x8051
rgba8 :: GLint
rgba8 = 0x8058 :: GLint
unsignedByte :: GLenum
unsignedByte = 0x1401 :: GLenum
textureWrapS :: GLenum
textureWrapS = 0x2802 :: GLenum
textureWrapT :: GLenum
textureWrapT = 0x2803 :: GLenum

newtype TextureObject = TextureObject { textureID :: GLuint }

loadTexture :: FilePath -> IO (Either String Main.TextureObject)
loadTexture path = do
  x <- loadPNGFile path
  case x of
    Left e -> return $ Left e
    Right img -> do
      texId <- alloca $ \texIdPtr -> do
        glGenTextures 1 texIdPtr
        texId <- peek texIdPtr
        glBindTexture gl_TEXTURE_2D texId
        glTexParameteri gl_TEXTURE_2D gl_TEXTURE_BASE_LEVEL 0
        glTexParameteri gl_TEXTURE_2D gl_TEXTURE_MAX_LEVEL 0
        let (w, h) = dimensions img
            (pif, pf) = if hasAlphaChannel img then (rgba8, gl_RGBA) else (rgb8, gl_RGB)
        withStorableArray (imageData img) $ \ptr ->
          glTexImage2D gl_TEXTURE_2D 0 pif (fromIntegral w) (fromIntegral h) 0 pf unsignedByte ptr
        glBindTexture gl_TEXTURE_2D 0
        return texId;
      return . Right $ Main.TextureObject texId

unloadTexture :: Main.TextureObject -> IO ()
unloadTexture tex = do
  withArrayLen [(textureID tex)] $ glDeleteTextures . fromIntegral

textureUnit :: GLuint -> GLenum
textureUnit x = 0x84c0 + x

-- | The camera object.
data Camera = Camera
  { pos :: Vec3 GLfloat
  , target :: Vec3 GLfloat
  , up :: Vec3 GLfloat
  }

data GameData = GameData
  { window :: GLFW.Window
  , env :: Mesh.Environment
  , camera :: Camera
  , cubeMesh :: Mesh.Object
  , cur :: (Double, Double) -- for mouse movement.
  , ballVector :: Vec2 GLfloat
  , ballSpeed :: GLfloat
  , level :: Int
  , score :: Int
  , restOfBall :: Int
  , actorList :: [Actor]
  , asciiShader :: Shader.Program
  , asciiSampler :: GLuint
  , asciiTexLocation :: Shader.UniformLocation
  , asciiTex :: Main.TextureObject
  }

initialBallVector :: Vec2 GLfloat
initialBallVector = V.normalize $ vec2 1 1

initialBallSpeed :: GLfloat
initialBallSpeed = 4

maxInitialBallSpeed :: GLfloat
maxInitialBallSpeed = 8

maxBallSpeed :: GLfloat
maxBallSpeed = 16

curLevelBallSpeed :: Int -> GLfloat
curLevelBallSpeed lv = initialBallSpeed + (min maxInitialBallSpeed $ 0.2 * (fromIntegral lv))

-- | The main loop in the game.
renderingLoop :: GLFW.Window -> Mesh.Object -> IO ()
renderingLoop window cubeMesh = do
  GLFW.setCursorInputMode window GLFW.CursorInputMode'Disabled
  curPos <- GLFW.getCursorPos window

  asciiProgram <- Shader.load 
    [ Shader.Info gl_VERTEX_SHADER (Shader.FileSource "shaders/bitmapfont.vert")
    , Shader.Info gl_FRAGMENT_SHADER (Shader.FileSource "shaders/bitmapfont.frag")
    ]
  asciiTexLoc <- Shader.uniformLocation asciiProgram "texSampler"

  sampler <- allocaArray 1 $ \buf -> do
    glGenSamplers 1 buf
    r <- peekArray 1 buf
    return $ Prelude.head r
  glSamplerParameteri sampler gl_TEXTURE_MIN_FILTER (fromIntegral gl_LINEAR)
  glSamplerParameteri sampler gl_TEXTURE_MAG_FILTER (fromIntegral gl_LINEAR)
  glSamplerParameteri sampler gl_TEXTURE_WRAP_S (fromIntegral gl_REPEAT)
  glSamplerParameteri sampler gl_TEXTURE_WRAP_T (fromIntegral gl_REPEAT)

  eitherTex <- loadTexture "data/ascii.png"
  case eitherTex of
    Left e -> hPutStrLn stderr e
    Right _ -> hPutStrLn stderr "success loading texture."
  let (Right tex) = eitherTex
  menv <- Mesh.createEnvironment
  loop initTitleScene Main.GameData
    { window = window
    , env = menv
    , camera = Camera
        { pos = vec3 (75) (-100) (-450)
        , target = vec3 (75) (150) (0)
        , up = vec3 0 1 0
        }
    , cubeMesh = cubeMesh
    , cur = curPos
    , ballVector = initialBallVector
    , ballSpeed = initialBallSpeed
    , level = 1
    , score = 0
    , restOfBall = 3
    , actorList = initialActors cubeMesh
    , asciiShader = asciiProgram
    , asciiSampler = sampler
    , asciiTexLocation = asciiTexLoc
    , asciiTex = tex
    }

keyAction window key taction faction = do
  keyState <- GLFW.getKey window key
  if (keyState == GLFW.KeyState'Pressed) then taction else faction

loop :: (GameData -> IO ()) -> GameData -> IO ()
loop scene gameData = do
  isExit <- GLFW.getKey (window gameData) GLFW.Key'Escape
  when (isExit /= GLFW.KeyState'Pressed) $
    (threadDelay 10000) >> (GLFW.windowShouldClose (window gameData)) >>= (flip unless) (scene gameData)

initTitleScene :: GameData -> IO ()
initTitleScene gameData = do
  let identityMatrix = V.identity :: Mat44 CFloat
      actors = makeActors
        (cubeMesh gameData) { Mesh.material = titleMaterials !! 0 }
        (V.scale (vec4 0.5 0.25 0.5 1) identityMatrix)
        $ TitleData.positions
      shadowActors = makeActors
        (cubeMesh gameData) { Mesh.material = titleMaterials !! 1 }
        (V.translate (vec3 (5) (-25) 0) $ V.scale (vec4 0.5 0.25 0.01 1) identityMatrix)
        $ TitleData.positions
  loop (titleScene (shadowActors ++ actors)) gameData

titleScene :: [Actor] -> GameData -> IO ()
titleScene actors gameData = do
  glClearColor 0.1 0.4 0.2 1
  glClear $ gl_COLOR_BUFFER_BIT .|. gl_DEPTH_BUFFER_BIT
  display
    (env gameData)
    Camera
      { pos = vec3 0 (-150) (-250)
      , target = vec3 0 50 0
      , up = vec3 0 1 0
      }
    actors
  drawAscii gameData (vec2 (-0.3) (-0.5)) (vec2 0.05 0.1) (Color4 1 1 1.0 1) "PUSH SPACE KEY"
  GLFW.swapBuffers $ window gameData
  GLFW.pollEvents
  keyAction (window gameData) GLFW.Key'Space
    (loop levelScene gameData)
    (loop (titleScene actors) gameData)

initGameOverScene :: GameData -> IO ()
initGameOverScene gameData = do
  start <- getCurrentTime
  loop (gameOverScene start) gameData

gameOverScene start gameData = do
  displayLevel gameData
  drawAscii gameData (vec2 (-0.8) 0.0) (vec2 0.15 0.3) (Color4 0 0 0 1) "GAME OVER"
  GLFW.swapBuffers $ window gameData
  GLFW.pollEvents
  now <- getCurrentTime
  keyAction (window gameData) GLFW.Key'Space (putStrLn "on") (return ())
  if realToFrac (diffUTCTime now start) < (3.0 :: Double)
  then do loop (gameOverScene start) gameData
  else do
    loop initTitleScene gameData
      { ballVector = initialBallVector
      , ballSpeed = initialBallSpeed
      , level = 1
      , score = 0
      , restOfBall = 3
      , actorList = initialActors $ cubeMesh gameData
      }

initMissScene :: GameData -> IO ()
initMissScene gameData = do
  start <- getCurrentTime
  loop (missScene start) gameData

missScene start gameData = do
  displayLevel gameData
  drawAscii gameData (vec2 (-0.4) 0.0) (vec2 0.1 0.2) (Color4 1 0.2 0.1 1) "MISS"
  GLFW.swapBuffers $ window gameData
  GLFW.pollEvents
  now <- getCurrentTime
  if realToFrac (diffUTCTime now start) < (3.0 :: Double)
  then loop (missScene start) gameData
  else do
    let (paddle:ball:others) = actorList gameData
        (px :. _ :. _) = Main.position paddle
    loop levelScene gameData
      { ballVector = initialBallVector
      , ballSpeed = curLevelBallSpeed $ level gameData
      , actorList = paddle : ball { Main.position = vec3 px 100 (-30) } : others
      }

initLevelClearScene :: GameData -> IO ()
initLevelClearScene gameData = do
  start <- getCurrentTime
  loop (levelClearScene start) gameData

levelClearScene :: UTCTime -> GameData -> IO ()
levelClearScene start gameData = do
  displayLevel gameData
  drawAscii gameData (vec2 (-0.4) 0.0) (vec2 0.1 0.2) (Color4 0.1 0.2 1.0 1) "CLEAR"
  GLFW.swapBuffers $ window gameData
  GLFW.pollEvents
  now <- getCurrentTime
  if realToFrac (diffUTCTime now start) < (3.0 :: Double)
  then loop (levelClearScene start) gameData
  else do
    loop levelScene gameData
      { ballVector = initialBallVector
      , ballSpeed = curLevelBallSpeed $ level gameData
      , level = (level gameData) + 1
      , actorList = initialActors $ cubeMesh gameData
      }

levelScene :: GameData -> IO ()
levelScene gameData = do
  let actors = actorList gameData
  displayLevel gameData
  GLFW.swapBuffers $ window gameData
  GLFW.pollEvents

  (newX, newY) <- GLFW.getCursorPos $ window gameData
  let (prevX, _) = cur gameData
      delta = realToFrac ((newX - prevX) * (-1)) :: GLfloat
      paddle = actors !! 0
      (x :. y :. z :. ()) = Main.position paddle
      newPaddleX = max 20 . min 230 $ (x + delta)

  let ball = actors !! 1
      blocks = Prelude.drop 78 actors
      (vecX :. vecY :. ()) = ballVector gameData
      (bx :. by :. _ :. ()) = Main.position ball
      (newBallX, newVecX) = boundWall (bx + vecX) vecX 270 (-20)
      (newBallY, newVecY) = boundWall (by + vecY) vecY 370 (-20)
      (newBall, newSpeedX' :. newSpeedY' :. (), newSpeed) = boundPaddle paddle (ball, vec2 newVecX newVecY, ballSpeed gameData)
      (hitX, hitY, nonHitBlocks) =
        intersectBlock (bx, by, (bx + newVecX * newSpeed), (by + newVecY * newSpeed)) blocks
      newActors =
        ( paddle { Main.position = vec3 newPaddleX y z  }
        : newBall
        : Prelude.take 76 (Prelude.drop 2 actors)
        ) ++ nonHitBlocks
      hasMiss = (\(_:.ny:._:.()) -> ny <= y - 25) (Main.position newBall) :: Bool

  let newGameData = gameData
        { cur = (newX, newY)
        , ballVector = vec2 (if hitX then (-newSpeedX') else newSpeedX') (if hitY then (-newSpeedY') else newSpeedY')
        , ballSpeed = (ballSpeed gameData) + (if newSpeed /= (ballSpeed gameData) then 0.1 else 0)
        , score = (score gameData) + if hitX || hitY then 1 else 0
        , restOfBall = (restOfBall gameData) - (if hasMiss then 1 else 0)
        , actorList = newActors
        }

  if hasMiss
  then
    if restOfBall gameData > 1
    then loop initMissScene newGameData
    else loop initGameOverScene newGameData
  else
    if Prelude.length blocks > 0
    then loop levelScene newGameData
    else loop initLevelClearScene newGameData

boundWall :: GLfloat -> GLfloat -> GLfloat -> GLfloat -> (GLfloat, GLfloat)
boundWall ballPos speed top bottom =
  let n = ballPos + speed
  in
    if n >= top
    then (n - (n - top), (-speed))
    else
      if n <= bottom
      then (n + (bottom - n), (-speed))
      else (n, speed)

boundPaddle :: Actor -- | paddle actor.
            -> (Actor, Vec2 GLfloat, GLfloat) -- | ball actor, ball vector and speed.
            -> (Actor, Vec2 GLfloat, GLfloat) -- | result of new ball actor, ball vector and speed.
boundPaddle paddle (ball, vx :. vy :. (), speed) =
  -- if ball y vector is upward, don't check collision between paddle.
  if vy >= 0
  then
    ( ball { Main.position = vec3 (bx + vx * speed) (by + vy * speed) bz }
    , vec2 vx vy
    , speed
    )
  else
    case result of
      Nothing ->
        ( ball { Main.position = vec3 (bx + vx * speed) (by + vy * speed) bz }
        , vec2 vx vy
        , speed
        )
      Just (hx, hy) ->
        ( ball { Main.position = vec3 hx hy bz }
        , vec2 (-(cos ((hx - (px - 60)) / 120 * pi))) (sin ((hx - (px - 60)) / 120 * pi))
        , speed - sqrt (((hx - bx) ** 2) + ((hy - by) ** 2))
        )
  where
    result :: Maybe (GLfloat, GLfloat)
    result = Collision.intersection paddleLine ballLine
    (bx :. by :. bz :. ()) = Main.position ball
    (px :. py :. _ :. ()) = Main.position paddle
    pLeft = px - 50
    pRight = px + 50
    pTop = py + 10
    paddleLine = (pLeft, pTop, pRight, pTop)
    ballLine = (bx, by, bx + vx * speed, by + vy * speed)

intersectBlock :: Collision.Line GLfloat -> [Actor] -> (Bool, Bool, [Actor])
intersectBlock ballLine@(x0, y0, x1, y1) blocks =
  intersectBlock' ballLine $ sortBlocks (x0, y0) blocks

intersectBlock' :: Collision.Line GLfloat -> [Actor] -> (Bool, Bool, [Actor])
intersectBlock' _ [] = (False, False, [])
intersectBlock' ballLine@(bx0, by0, bx1, by1) (blockActor : xs) =
  if hitB || hitT
  then (inheritHitX, (hitB || hitT || inheritHitY), nonHitBlocks)
  else
    if hitL || hitR
    then ((hitL || hitR || inheritHitX), inheritHitY, nonHitBlocks)
    else (inheritHitX, inheritHitY, blockActor : nonHitBlocks)
  where
    (x :. y :. _ :. ()) = Main.position blockActor
    (halfW, halfH) = (25, 12.5)
    hitB = hitOnSide (by0 < by1) ((x + halfW), (y - halfH), (x - halfW), (y - halfH))
    hitT = hitOnSide (by0 > by1) ((x - halfW), (y + halfH), (x + halfW), (y + halfH))
    hitL = hitOnSide (bx0 < bx1) ((x - halfW), (y - halfH), (x - halfW), (y + halfH))
    hitR = hitOnSide (bx0 > bx1) ((x + halfW), (y + halfH), (x + halfW), (y - halfH))
    hitOnSide :: Bool -> Collision.Line GLfloat -> Bool
    hitOnSide active side = if active
      then maybe False (\_ -> True) $ Collision.intersection ballLine side
      else False
    (inheritHitX, inheritHitY, nonHitBlocks) = intersectBlock' ballLine xs

-- | Sorting the blocks for the distance from center of the ball.
sortBlocks :: (GLfloat, GLfloat) -> [Actor] -> [Actor]
sortBlocks (x, y) blocks =
  Prelude.map snd . sortBy (comparing fst) $ Prelude.map (\block -> (func block, block)) blocks
  where
    func a = (\(bx :. by :. _ :. ()) -> sqrt ((bx - x) ** 2 + (by - y) ** 2)) $ Main.position a

-- | Make the view matrix looking at any point.
lookAt :: Floating a => Vec3 a -> Vec3 a -> Vec3 a -> Mat44 a
lookAt eyePos targetPos upVec = x :. y :. z :. h :. ()
  where
    forward = V.normalize $ targetPos - eyePos
    right = V.normalize $ V.cross forward upVec
    up' = V.cross right forward
    x = V.snoc right (-(V.dot right eyePos))
    y = V.snoc up' (-(V.dot up' eyePos))
    z = V.snoc (-forward) (V.dot forward eyePos)
    h = 0 :. 0 :. 0 :. 1 :. ()

stringToTexCoord :: String -> [(GLfloat, GLfloat)]
stringToTexCoord str =
  zip (Prelude.map (u . ord) str) (Prelude.map ((+(1/256)) . v . ord) str)
  where
    u = (/ 16) . fromIntegral . (`mod` 16)
    v = (/ 16) . fromIntegral . (`div` 16)

-- 0---2---4---
-- | / | / |
-- 1---3---5---
stringToVertices :: String -> (GLfloat, GLfloat) -> (GLfloat, GLfloat) -> (GLfloat, GLfloat, GLfloat, GLfloat) -> [GLfloat]
stringToVertices str (offx, offy) (sx, sy) (r, g, b, a) =
  Prelude.foldl1 (++) vertices
  where
    vertices = [ makeQuad x offy c u v
      | c <- [[r, g, b, a]]
      , (x, (u, v)) <- zip xPositions (stringToTexCoord str)
      ]
    makeQuad x y c u v = []
      ++ [x     , y + sy, 0] ++ c ++ [u'        , v]
      ++ [x     , y     , 0] ++ c ++ [u'        , v + unitV]
      ++ [x + sx, y + sy, 0] ++ c ++ [u' + unitU, v]
      ++ [x + sx, y     , 0] ++ c ++ [u' + unitU, v + unitV]
      where
       u' = u + (1/256)
    xPositions = Prelude.take (Prelude.length str) [offx, (offx + sx) .. ]
    unitU :: GLfloat
    unitU = 1 / 16 - (1/256)
    unitV :: GLfloat
    unitV = 1 / 16 - (1/256)

-- | Render level.
displayLevel :: GameData -> IO ()
displayLevel gameData = do
  glClearColor 0.1 0.4 0.2 1
  glClear $ gl_COLOR_BUFFER_BIT .|. gl_DEPTH_BUFFER_BIT
  display (env gameData) (camera gameData) (actorList gameData)
  drawAscii gameData (vec2 0.5 0.7) fontScale fontColor "[SCORE]"
  drawAscii gameData (vec2 0.5 0.6) fontScale fontColor . printf "%05d00" $ score gameData
  drawAscii gameData (vec2 0.5 0.4) fontScale fontColor "[BALL]"
  when (restOfBall gameData > 0) $
    drawAscii gameData (vec2 0.5 0.3) fontScale fontColor . Prelude.take (restOfBall gameData) $ repeat 'o'
  drawAscii gameData (vec2 0.5 0.1) fontScale fontColor "[LEVEL]"
  drawAscii gameData (vec2 0.5 0.0) fontScale fontColor . printf "%03d" $ level gameData
  where
    fontScale :: Scale2
    fontScale = (vec2 0.05 0.1)
    fontColor :: Color4 GLfloat
    fontColor = (Color4 0.95 0.95 1 1)


-- | Render all of the actors.
display :: Mesh.Environment -> Camera -> [Actor] -> IO ()
display env cam actors = do
  let viewMatrix = Main.lookAt (pos cam) (target cam) (up cam)
      projMatrix = (V.perspective 0.1 2000 (pi / 4) (4 / 3)) :: Mat44 GLfloat
      newLS = Prelude.map (toViewSpace viewMatrix) lightSource
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
  mapM_
    (\(Actor mesh mat actorPos) -> do
      Mesh.draw env mesh (V.translate actorPos mat) viewMatrix projMatrix
    )
    actors

drawAscii :: GameData -> Offset2 -> Scale2 -> Color4 GLfloat -> String -> IO ()
drawAscii gameData (ox :. oy :. ()) (sx :. sy :. ()) (Color4 r g b a) str = do
  let vertices = stringToVertices str (ox, oy) (sx, sy) (r, g, b, a)
  vao <- genObjectName
  vb <- Mesh.createBuffer ArrayBuffer vertices
  let numPositionElements = 3
      numColorElements = 4
      numTexCoordElements = 2
      vPosition = AttribLocation 0
      vNormal = AttribLocation 1
      vColor = AttribLocation 2
      vTexCoord = AttribLocation 3
      offsetPosition = 0
      offsetColor = offsetPosition + numPositionElements
      offsetTexCoord = offsetColor + numColorElements
      sizeElement = sizeOf $ Prelude.head vertices
      sizeVertex = fromIntegral $ sizeElement * (numPositionElements + numColorElements + numTexCoordElements)
  bindVertexArrayObject $= Just vao
  bindBuffer ArrayBuffer $= Just vb
  vertexAttribPointer vPosition $=
    ( ToFloat
    , VertexArrayDescriptor (fromIntegral numPositionElements) Float sizeVertex (Mesh.bufferOffset (offsetPosition * sizeElement))
    )
  vertexAttribArray vPosition $= Enabled
  vertexAttribArray vNormal $= Disabled
  vertexAttribPointer vColor $=
    ( ToFloat
    , VertexArrayDescriptor (fromIntegral numColorElements) Float sizeVertex (Mesh.bufferOffset (offsetColor * sizeElement))
    )
  vertexAttribArray vColor $= Enabled
  vertexAttribPointer vTexCoord $=
    ( ToFloat
    , VertexArrayDescriptor (fromIntegral numTexCoordElements) Float sizeVertex (Mesh.bufferOffset (offsetTexCoord * sizeElement))
    )
  vertexAttribArray vTexCoord $= Enabled
  bindVertexArrayObject $= Nothing

  glActiveTexture (textureUnit 0)
  glBindTexture gl_TEXTURE_2D $ textureID $ asciiTex gameData
  glBindSampler 0 $ asciiSampler gameData
  glUniform1i (asciiTexLocation gameData) (fromIntegral $ textureUnit 0)

  Shader.useProgram $ Just (asciiShader gameData)
  bindVertexArrayObject $= Just vao

  GLRaw.glEnable gl_BLEND
  GLRaw.glBlendFunc gl_SRC_ALPHA gl_ONE_MINUS_SRC_ALPHA
  GLRaw.glFrontFace gl_CCW
  drawArrays TriangleStrip 0 $ fromIntegral ((Prelude.length str) * 4)
  GLRaw.glFrontFace gl_CW

  bindVertexArrayObject $= Nothing
  Shader.useProgram Nothing
  glBindSampler 0 0
  glBindTexture gl_TEXTURE_2D 0

  vertexAttribArray vTexCoord $= Disabled
  deleteObjectName vb
  deleteObjectName vao
