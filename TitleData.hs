module TitleData
  ( positions
  , fontColor
  , fontSubColor
  )
where

import Graphics.Rendering.OpenGL (Color4(..), GLfloat)
import Foreign.C.Types

positions :: [(CFloat, CFloat, CFloat)]
positions =
  foldl1 (++) $ map
    (\(xs, y) -> foldr (toPosition y) [] $ zip xs [0, 1 ..])
    (zip renderingData [0, 1 ..])
  where
    toPosition :: CFloat -> (Char, CFloat) -> [(CFloat, CFloat, CFloat)] -> [(CFloat, CFloat, CFloat)]
    toPosition y (c, x) lst
      | c == '0' || c == '1' = ((-x) * 10 + 150, (-y) * 5 + 100, -30) : lst
      | otherwise = lst

renderingData :: [[Char]]
renderingData =
  [ "11  00  000  0  0 0   1  0 0 000"
  , "111 000 000 000 0 0  111 0 0 000"
  , "1 1 0 0 0   0 0 00   1 1 0 0  0 "
  , "1 1 0 0 0   0 0 00   1 1 0 0  0 "
  , "11  000 000 000 0    1 1 0 0  0 "
  , "111 00  000 000 0    1 1 0 0  0 "
  , "1 1 0 0 0   0 0 00   1 1 0 0  0 "
  , "1 1 0 0 0   0 0 00   1 1 0 0  0 "
  , "111 0 0 000 0 0 0 0  111 000  0 "
  , "11  0 0 000 0 0 0 0   1   0   0 "
  ]

fontColor :: Color4 GLfloat
fontColor = Color4 0.1 0.3 0.7 1.0

fontSubColor :: Color4 GLfloat
fontSubColor = Color4 0.7 0.3 0.1 1.0

