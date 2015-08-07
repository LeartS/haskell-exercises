{-
Problem 13 of chapter 3
=======================

13. Using the code from the preceding three exercises, implement Graham’s scan
algorithm for the convex hull of a set of 2D points. You can find good
description of what a convex hull (http://en.wikipedia.org/wiki/Convex_hull) is,
and how the Graham scan algorithm (http://en.wikipedia.org/wiki/Graham_scan)
should work, on Wikipedia (http://en.wikipedia.org/).
-}

import Data.List
import Data.Function
import Debug.Trace (trace)

import Directions

-- For the graham scan we need to consider y first and x as "tiebraker"
instance Ord Point where
  compare a b | y(a) == y(b) = compare (x a) (x b)
              | otherwise = compare (y a) (y b)

norm :: Point -> Double
norm (Point x y) = sqrt (x^2 + y^2)

difference :: Point -> Point -> Point
difference a b = Point (x(a) - x(b)) (y(a) - y(b))

-- Actually returns the negative of the cosine of the angle,
-- which when sorted by it returns the same results as sorting by the angle
angle :: Point -> Point -> Double
angle a b | den == 0 = (-1)
          | otherwise = (x(a)*x(b) + y(a)*y(b)) / (-den)
    where den = norm(a) * norm(b)

-- Not bad function composition for my third day of haskell and
-- functional programming, if I may say so myself!
comparisonFunction :: Point -> Point -> Point -> Ordering
comparisonFunction ref a b
  | angleComparison == EQ = compare (x a) (x b)
  | otherwise = angleComparison
  where angleComparison = (compare `on` ((angle (Point 1 0)) . (`difference` ref))) a b

sortedByAngle :: [Point] -> [Point]
sortedByAngle points = sortBy (comparisonFunction reference) points
  where reference = minimum points

convexHull' :: Point -> [Point] ->  [Point]
-- convexHull' c k@(b:a:xs) | trace ("convexhull' " ++ show c ++ show b ++ " " ++ show a ++ show xs) False = undefined
convexHull' c k@(b:a:xs) | direction == LeftTurn = c:b:a:xs
                       | direction == RightTurn = c:a:xs
  where direction = calculateDirection a b c
convexHull' c xs = c:xs

convexHull :: [Point] -> [Point]
convexHull points = foldr convexHull' [] (reverse . sortedByAngle $ points)
