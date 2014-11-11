module Collision
  ( intersection
  , hasIntersection
  , Line
  , Point
  , Vec2
  )
where

type Line a = (a, a, a, a)
type Point a = (a, a)
type Vec2 a = (a, a)

intersection :: (Show a, Floating a, Ord a) => Line a -> Line a -> Maybe (Point a)
intersection (bx, by, cx, cy) (px, py, qx, qy) =
  if (c_v1_v2 == 0) || (t1 + eps < 0) || (t1 - eps > 1) || (t2 + eps < 0) || (t2 - eps > 1)
  then Nothing
  else Just $ (bx + v1x * t1, by + v1y * t1)
  where
    eps = 0.000001
    v = (px - bx, py - by)
    v1@(v1x, v1y) = (cx - bx, cy - by)
    v2 = (qx - px, qy - py)
    c_v1_v2 = cross v1 v2
    c_v_v1 = cross v v1
    c_v_v2 = cross v v2
    t1 = c_v_v2 / c_v1_v2
    t2 = c_v_v1 / c_v1_v2

hasIntersection :: (Show a, Floating a, Ord a) => Line a -> Line a -> Bool
hasIntersection (bx, by, cx, cy) (px, py, qx, qy) =
  if ((bx - cx) * (py - by) + (by - cy) * (bx - px)) * ((bx - cx) * (qy - by) + (by - cy) * (bx - qx)) < 0
  then
    if ((px - qx) * (by - py) + (py - qy) * (px - bx)) * ((px - qx) * (cy - py) + (py - qy) * (px - cx)) < 0
    then True
    else False
  else False

cross :: (Floating a) => Vec2 a -> Vec2 a -> a
cross (ax, ay) (bx, by) = ax * by - ay * bx
