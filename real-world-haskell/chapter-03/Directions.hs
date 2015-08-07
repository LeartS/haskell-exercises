{-
Problems 10, 11 and 12 of chapter 3
===================================

10. Consider three two-dimensional points, a, b, and c. If we look at the angle
formed by the line segment from a to b and the line segment from b to c,
it turns left, turns right, or forms a straight line.
Define a Direction data type that lets you represent these possibilities.

11. Write a function that calculates the turn made by three two-dimensional
points and returns a Direction.

12. Define a function that takes a list of two-dimensional points and computes
the direction of each successive triple. Given a list of points [a,b,c,d,e],
it should begin by computing the turn made by [a,b,c], then the turn made
by [b,c,d], then [c,d,e]. Your function should return a list of Direction.
-}

module Directions (
  Point(..),
  Direction(..),
  calculateDirection,
  directionList
  ) where

data Point = Point {
  x :: Double,
  y :: Double
  } deriving (Eq, Show)

data Direction = LeftTurn | RightTurn | Straight
                 deriving (Eq, Show)

calculateDirection :: Point -> Point -> Point -> Direction
calculateDirection a b c | det < 0 = RightTurn
                         | det > 0 = LeftTurn
                         | det == 0 = Straight
  -- Determinant of the two difference vectors
  where det = (x(b) - x(a)) * (y(c) - y(b)) -
              (x(c) - x(b)) * (y(b) - y(a))

directionList :: [Point] -> [Direction]
directionList (a:b:c:[]) = (calculateDirection a b c) : []
directionList (a:b:c:others) = (calculateDirection a b c) : (directionList (b : c : others))
