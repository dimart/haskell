module GrahamAlg where

import Data.List (sortBy)
import Data.List (minimumBy)
import Data.List (delete)

--Consider three two-dimensional points, a, b, and c. If we look at the angle formed
--by the line segment from a to b and the line segment from b to c, it turns left, turns
--right, or forms a straight line. Define a Direction data type that lets you represent
--these possibilities.
--11. Write a function that calculates the turn made by three two-dimensional points
--and returns a Direction.

data Direction = TurnsLeft
			   | TurnsRight
			   | StraightLine
			   deriving (Show, Eq)

data Point2D = Point2D 
				{
					x :: Double, 
					y :: Double 
				} 
				deriving (Show, Eq)

--Для определения, образуют ли три точки a, b и c левый поворот, можно использовать обобщение векторного произведения на двумерное пространство, 
--а именно условие левого поворота будет выглядеть следующим образом: 
--u_x v_y - u_y v_x > 0 , где  u = \left\{ b_x - a_x, \; b_y - a_y \right\}, v = \left\{ c_x - a_x, \; c_y - a_y \right\}

direction :: Point2D -> Point2D -> Point2D -> Direction
direction a b c 
	| indicator <  0 = TurnsRight
	| indicator == 0 = StraightLine
	| indicator >  0 = TurnsLeft
	where 
		indicator = (x b - x a) * (y c - y a) - (y b - y a) * (x c - x a) 

--Define a function that takes a list of two-dimensional points and computes the
--direction of each successive triple. Given a list of points [a,b,c,d,e], it should
--begin by computing the turn made by [a,b,c], then the turn made by [b,c,d],
--then [c,d,e]. Your function should return a list of Direction.

directionList :: [Point2D] -> [Direction]
directionList (a:b:c:pts) = (direction a b c) : directionList (b:c:pts) 
directionList _ = []

listOfPoints = [(Point2D 0 0), (Point2D 4 4), (Point2D 8 8), (Point2D 10 4), (Point2D 12 12)] 
-- > directionList listOfPoints 
-- should return 
-- > [StraightLine, TurnsRight, TurnsLeft]

--Graham scan algorithm
cmpPoints :: Point2D -> Point2D -> Ordering
cmpPoints (Point2D ax ay) (Point2D bx by)
	| ay < by  = LT
	| ay > by  = GT
	| ay == by = compare ax bx 

polarAngle :: Point2D -> Point2D -> Double
polarAngle (Point2D ax ay) (Point2D bx by) = 
	(sqrt ((ay - by)^2 + (ax - bx)^2)) / (ax - bx)

compareAnglsWith :: Point2D -> Point2D -> Point2D -> Ordering
compareAnglsWith p a b =
	compare (polarAngle p a) (polarAngle p b) 

findConvexHull :: [Point2D] -> [Point2D]
findConvexHull pts 
	| length' pts >= 3 = calc $ sort pts
	| otherwise        = []
	where
		calc (a:b:c:smth) 
			| direction a b c == TurnsLeft = findConvexHull (b:c:smth)
			| otherwise                    = findConvexHull (a:c:smth)
		calc (x:y:_)                       = [x, y]
		sort pts = let p0 = minimumBy cmpPoints pts
				   in  p0 : (sortBy (compareAnglsWith p0) (delete p0 pts))

test = findConvexHull [(Point2D 0 0), (Point2D 2 2), (Point2D 2 0), (Point2D 0 2)]
